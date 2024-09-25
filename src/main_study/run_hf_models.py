"""Run HF models on either natural or synthetic affordance images."""

import pandas as pd
import os.path as op
import seaborn as sns
import numpy as np
from scipy.spatial.distance import cosine

from tqdm import tqdm

from transformers import CLIPProcessor, CLIPModel, FlavaProcessor, FlavaModel, BlipProcessor, BlipModel, AlignProcessor, AlignModel
from transformers import ViltForImageAndTextRetrieval, ViltProcessor, ViltConfig, BridgeTowerForImageAndTextRetrieval, BridgeTowerProcessor
import torch
from PIL import Image
from torch.nn.functional import cosine_similarity
import torch.nn.functional as F






### Possible models
MODELS = {
    'clip-vit-base-patch32': ['openai/clip-vit-base-patch32', CLIPModel, CLIPProcessor],
    'clip-vit-large-patch14': ['openai/clip-vit-large-patch14', CLIPModel, CLIPProcessor],
    'flava-full': ['facebook/flava-full', FlavaModel, FlavaProcessor],
    'align-base': ['kakaobrain/align-base', AlignModel, AlignProcessor],
    'clip-huge-14': ['laion/CLIP-ViT-H-14-laion2B-s32B-b79K', CLIPModel, CLIPProcessor],
    'clip-giant': ['laion/CLIP-ViT-g-14-laion2B-s12B-b42K', CLIPModel, CLIPProcessor],
    'clip-big-giant': ['laion/CLIP-ViT-bigG-14-laion2B-39B-b160k', CLIPModel, CLIPProcessor],
    'vilt-coco': ['dandelin/vilt-b32-finetuned-coco', ViltForImageAndTextRetrieval, ViltProcessor],
    'vilt-f30k': ['dandelin/vilt-b32-finetuned-flickr30k', ViltForImageAndTextRetrieval, ViltProcessor],
    'bridgetower': ['BridgeTower/bridgetower-large-itm-mlm-itc', BridgeTowerForImageAndTextRetrieval, BridgeTowerProcessor]
    ### TODO: ViLT? (Would need to use logits instead)? Bridgetower?
    ##### Issue with VILT is context window isn't long enough?
    ### TODO: BLIP for image-text-matching instead? There are issues with BlipModel class...
    ### TODO: output_hidden_states for probing?
    ### Details on Flava: "For example, FLAVA... (from Zhang et al., 2024)"
    }



def count_parameters(model):
    """credit: https://stackoverflow.com/questions/49201236/check-the-total-number-of-parameters-in-a-pytorch-model"""
    
    total_params = 0
    for name, parameter in model.named_parameters():
        
        # if the param is not trainable, skip it
        if not parameter.requires_grad:
            continue
        
        # otherwise, count it towards your number of params
        params = parameter.numel()
        total_params += params
    print(f"Total Trainable Params: {total_params}")
    
    return total_params
    


class HFModelRunner(object):



    def __init__(self, model_name, version):
        self.model_name = model_name
        self.model_path = MODELS[model_name][0]
        self.version = version 

        self.load_model(model_name)
        self.load_dataset(version)

    def load_model(self, model_name):

        classes = (MODELS[model_name][1], MODELS[model_name][2])

        self.processor = classes[1].from_pretrained(self.model_path)
        self.model = classes[0].from_pretrained(self.model_path)

        self.num_params = count_parameters(self.model)


    def load_dataset(self, version = "natural"):
        path = "data/stimuli/{x}/affordance_{y}.csv".format(x = version, y = version)
        self.df = pd.read_csv(path)


    def compare_inputs(self, text, image):

        if self.model_name in ['clip-vit-base-patch32', 'clip-vit-large-patch14', 
                              'blip-image-captioning-base', 'blip-image-captioning-large',
                              'align-base', 'clip-huge-14', 'clip-giant', 'clip-big-giant']:

            ## Encode text
            text_input = self.processor(text=text, return_tensors="pt", padding=True)

            ## Encode image
            image_input = self.processor(images=image, return_tensors="pt")

            # Get the image and text representations
            with torch.no_grad():
                image_features = self.model.get_image_features(**image_input)
                text_features = self.model.get_text_features(**text_input)

            # Normalize the features
            image_features = image_features / image_features.norm(dim=-1, keepdim=True)
            text_features = text_features / text_features.norm(dim=-1, keepdim=True)

            return cosine_similarity(image_features, text_features).item()


        elif self.model_name in ['flava-full']:

            ## Encode text
            text_input = self.processor(text=text, return_tensors="pt", padding=True)

            ## Encode image
            image_input = self.processor(images=image, return_tensors="pt")

            # Get the image and text representations
            with torch.no_grad():
                image_features = self.model.get_image_features(**image_input)
                text_features = self.model.get_text_features(**text_input)

            ### Get CLS embedding for FLAVA
            image_cls_embedding = image_features[:, 0, :]  # [batch_size, embed_dim]
            text_cls_embedding = text_features[:, 0, :]    # [batch_size, embed_dim]
            
            # Normalize the features
            image_cls_embedding = F.normalize(image_cls_embedding, dim=-1)
            text_cls_embedding = F.normalize(text_cls_embedding, dim=-1)

            return cosine_similarity(image_cls_embedding, text_cls_embedding).item()


        elif self.model_name in ['vilt-coco', 'vilt-f30k']:

            # Define and preprocess the text
            encoding = self.processor(image, text, return_tensors="pt")
    
            # Run through model
            with torch.no_grad():
                outputs = self.model(**encoding)
    
            # Return logits
            logits = outputs.logits[0, :].item()

            return logits

        elif self.model_name in ['bridgetower']:

            # Define and preprocess the text
            ### TODO: Requires truncation in some cases
            encoding = self.processor(image, text, return_tensors="pt", max_length=40, truncation=True)
    
            # Run through model
            with torch.no_grad():
                outputs = self.model(**encoding)
    
            # Return logits
            ### [0, 1] is logit for probability that image/text match
            logits = outputs.logits[0, 1].item()

            return logits

    def run_model(self):

        ### Track results
        results = []

        for index, row in tqdm(self.df.iterrows(), total=self.df.shape[0]):
            text = row['condition']

            for cond in ['afforded_image', 'non-afforded_image', 'related_image']:

                ## Get image path
                img_name = row[cond]
                path = "data/stimuli/{x}/images/".format(x = self.version)
                img_path = op.join(path, img_name)

                # Load and preprocess the image
                image = Image.open(img_path).convert("RGB")

                # Get features
                response = self.compare_inputs(text, image)

                # Response type
                response_type = 'logits' if self.model_name in ['vilt-coco', 'bridgetower'] else 'cosine_similarity'

                # track data
                results.append({
                    'text': text,
                    'condition': cond,
                    'img_name': img_name,
                    'group_id': row['group_id'],
                    'prompt_type': row['prompt_type'],
                    'response_type': response_type,
                    'response': response,
                    'model_name': self.model_name,
                    'version': self.version,
                    'num_params': self.num_params
                })


        # Turn results into DataFrame
        self.df_results = pd.DataFrame(results)
        
        # Save to .csv
        SAVE_PATH = "data/processed/models/hf_models/{model}_{version}_affordances.csv".format(model = self.model_name, version = self.version)
        print("Saving to " + SAVE_PATH)
        self.df_results.to_csv(SAVE_PATH, index = False)



### TODO: BLIP weights not initialized?

models_to_run = ['align-base',
                'clip-vit-base-patch32',
                'clip-vit-large-patch14',
                'flava-full',
                'clip-huge-14',
                'clip-giant',
                'clip-big-giant',
                'vilt-coco',
                'vilt-f30k',
                'bridgetower'
                ]
for model_name in models_to_run:
    print(model_name)
    for version in ['natural', 'synthetic']:
    
    # for version in ['synthetic']:
        hf = HFModelRunner(model_name = model_name, 
                           version = version)
        hf.run_model()

