import os
from PIL import Image

def add_north_arrow():
    # Paths
    current_dir = os.path.dirname(os.path.abspath(__file__))
    parent_dir = os.path.dirname(current_dir)
    north_arrow_path = os.path.join(current_dir, 'north.png')
    
    if not os.path.exists(north_arrow_path):
        print(f"Error: {north_arrow_path} not found.")
        return

    # Load north arrow
    north_arrow = Image.open(north_arrow_path).convert("RGBA")
    nw, nh = north_arrow.size

    # Iterate through images in parent directory
    for filename in os.listdir(parent_dir):
        if filename.lower().endswith(('.png', '.jpg', '.jpeg')):
            image_path = os.path.join(parent_dir, filename)
            
            # Skip the elements folder or any other directory
            if os.path.isdir(image_path):
                continue
                
            try:
                with Image.open(image_path) as img:
                    img = img.convert("RGBA")
                    w, h = img.size
                    
                    # Calculate top-left position
                    position = (0, 0)
                    
                    # Paste north arrow
                    img.paste(north_arrow, position, north_arrow)
                    
                    # Convert back to RGB if it was originally JPEG (to avoid transparency issues on save)
                    # or keep as RGBA if it's PNG.
                    if filename.lower().endswith(('.jpg', '.jpeg')):
                        img = img.convert("RGB")
                    
                    # Overwrite
                    img.save(image_path)
                    print(f"Updated: {filename}")
            except Exception as e:
                print(f"Failed to process {filename}: {e}")
        # break # For test purposes

if __name__ == "__main__":
    add_north_arrow()
