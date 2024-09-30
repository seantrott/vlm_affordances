"""Run GPT models."""

from openai import OpenAI
import openai
import pandas as pd
import os
import base64
from tqdm import tqdm

client = OpenAI()


# Function to encode the image
def encode_image(image_path):
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode("utf-8")


# Function to iterate and encode images in a folder
def encode_images_in_folder(folder_path):
    encoded_images = {}
    for image_name in os.listdir(folder_path):
        image_path = os.path.join(folder_path, image_name)
        if os.path.isfile(image_path):
            encoded_images[image_name] = encode_image(image_path)
    return encoded_images


# Function to generate URL for image
def generate_data_url(base64_image, image_name):
    if image_name.lower().endswith(".png"):
        mime_type = "image/png"
    else:
        mime_type = "image/jpeg"

    return f"data:{mime_type};base64,{base64_image}"


# Call to GPT4
import backoff  # for exponential backoff


@backoff.on_exception(backoff.expo, openai.RateLimitError)
def gpt4_vision_call(
    prompt, base64_image, image_name, model="gpt-4-turbo", scale_limit=7
):
    data_url = generate_data_url(base64_image, image_name)

    response = client.chat.completions.create(
        model=model,
        messages=[
            {
                "role": "system",
                "content": f"""In this task, you will read short passages and look at an image of an object.
Please rate how sensible it would be to take the action described in the last sentence using the object in
the image in the context of the whole passage. The scale goes from 1 (virtual nonsense) to {scale_limit} (completely sensible).
Be sure to read the sentences carefully. Please respond only with a number between 1 and {scale_limit}.
""",
            },
            {
                "role": "user",
                "content": [
                    {"type": "text", "text": prompt},
                    {
                        "type": "image_url",
                        "image_url": {
                            "url": data_url,
                        },
                    },
                ],
            },
        ],
        max_tokens=3,
        temperature=0,
    )
    return response.choices[0].message.content


def main(version, model_name="gpt-4-turbo", scale_limit=7):

    ### Set up paths
    csv_path = "data/stimuli/{x}/affordance_{y}.csv".format(x=version, y=version)
    img_folder = "data/stimuli/{x}/images".format(x=version)

    ### Load data
    df = pd.read_csv(csv_path)
    ### Encode images
    encoded_images_dict = encode_images_in_folder(img_folder)

    ### Run through GPT-4
    results = []
    for _, row in tqdm(df.iterrows(), total=df.shape[0]):
        # row = df.iloc[13]
        text = row["condition"]

        ### Each version/condition
        for image_type in ["afforded_image", "non-afforded_image", "related_image"]:
            # image_type = "non-afforded_image"
            ### Get corresponding image for this condition
            image_name = row[image_type]
            if image_name in encoded_images_dict:
                base64_image = encoded_images_dict[image_name]
                result = gpt4_vision_call(
                    text, base64_image, image_name, model_name, scale_limit=scale_limit
                )
            else:
                result = "Image encoding not found"

            results.append(
                {
                    "text": text,
                    "response": result,
                    "condition": image_type,
                    "img_name": image_name,
                    "response_type": "generation",
                    "group_id": row["group_id"],
                    "model_name": f"{model_name}_{scale_limit},
                    "version": version,
                    "prompt_type": row["prompt_type"],
                }
            )

    df_results = pd.DataFrame(results)

    # Save to .csv
    SAVE_PATH = f"data/processed/models/closed_models/{model_name}_{version}_{scale_limit}_affordances.csv"
    print("Saving to " + SAVE_PATH)
    df_results.to_csv(SAVE_PATH, index=False)


if __name__ == "__main__":
    MODELS = ["gpt-4o", "gpt-4-turbo"]
    SCALE_LIMIT = 100

    for mpath in MODELS:
        print(mpath)
        for version in ["natural", "synthetic"]:
            main(version, mpath, SCALE_LIMIT)
