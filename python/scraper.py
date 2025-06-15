import requests
from bs4 import BeautifulSoup
import re

def scrape_surf_wiki():
    """
    Scrapes content from a list of SURF wiki pages and saves it to a text file.
    """
    # Raw list of URLs provided by the user
    raw_urls_input = """
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/30660216/Connecting+to+the+system?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/92668034/Interacting+with+the+filesystem?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/30660217/Creating+and+running+jobs?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/74225195/Interactive+development+GPU+node?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/30660235/Software+development?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/1474857/Using+X+Window+Applications?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/101089402/Web-based+access+to+Snellius+OpenOnDemand?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/62227640/Available+datasets+and+models+on+Snellius?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/166920775/Snellius+and+sensitive+data?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/30660216/Connecting+to+the+system
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/92668034/Interacting+with+the+filesystem
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/30660217/Creating+and+running+jobs
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/74225195/Interactive+development+GPU+node
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/30660235/Software+development
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/1474857/Using+X+Window+Applications
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/101089402/Web-based+access+to+Snellius+OpenOnDemand
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/62227640/Available+datasets+and+models+on+Snellius
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/166920775/Snellius+and+sensitive+data
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/74227843/Applications?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/92668026/Containers?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/74227858/Energy+efficient+HPC?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/74227837/Integrated+Development+Environments+IDE?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/74227856/Machine+Learning?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/74227819/Performance+Analysis+Tools?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/92668029/Programming+Languages?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/30660266/Software?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/74227850/Visualisation?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/74227843/Applications
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/92668026/Containers
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/74227858/Energy+efficient+HPC
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/74227837/Integrated+Development+Environments+IDE
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/74227856/Machine+Learning
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/74227819/Performance+Analysis+Tools
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/92668029/Programming+Languages
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/30660266/Software
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/74227850/Visualisation
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/30660216/Connecting+to+the+system?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/92668034/Interacting+with+the+filesystem?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/30660217/Creating+and+running+jobs?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/74225195/Interactive+development+GPU+node?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/30660235/Software+development?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/1474857/Using+X+Window+Applications?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/101089402/Web-based+access+to+Snellius+OpenOnDemand?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/62227640/Available+datasets+and+models+on+Snellius?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/166920775/Snellius+and+sensitive+data?src=contextnavpagetreemode
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/30660216/Connecting+to+the+system
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/92668034/Interacting+with+the+filesystem
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/30660217/Creating+and+running+jobs
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/74225195/Interactive+development+GPU+node
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/30660235/Software+development
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/1474857/Using+X+Window+Applications
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/101089402/Web-based+access+to+Snellius+OpenOnDemand
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/62227640/Available+datasets+and+models+on+Snellius
    https://servicedesk.surf.nl/wiki/spaces/WIKI/pages/166920775/Snellius+and+sensitive+data
    """

    # Split the input string into individual URLs and remove leading/trailing whitespace
    urls_list = [url.strip() for url in raw_urls_input.strip().split('\n')]

    # De-duplicate URLs by removing query parameters (like ?src=...) and using a set
    unique_base_urls = set()
    for url in urls_list:
        base_url = url.split('?')[0] # Get the part of the URL before '?'
        unique_base_urls.add(base_url)

    output_filename = "scraped_content.txt"

    # Standard headers to mimic a browser request
    headers = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
    }

    with open(output_filename, "w", encoding="utf-8") as outfile:
        print(f"Starting to scrape {len(unique_base_urls)} unique pages. Output will be saved to {output_filename}\n")
        for i, url in enumerate(list(unique_base_urls)):
            print(f"Scraping ({i+1}/{len(unique_base_urls)}): {url} ...")
            try:
                # Make the HTTP GET request
                response = requests.get(url, headers=headers, timeout=15) # Added timeout
                response.raise_for_status()  # Raise an exception for HTTP errors (4xx or 5xx)

                # Parse the HTML content of the page
                soup = BeautifulSoup(response.content, "html.parser")

                # Find the main content div. Based on inspection, Confluence uses <div id="main-content">
                # and often the actual text is within a <div class="wiki-content"> inside it.
                # Let's try to be more specific if possible or fall back.
                content_area = soup.find("div", class_="wiki-content")
                if not content_area: # Fallback if 'wiki-content' class is not found
                    content_area = soup.find("div", id="main-content")


                if content_area:
                    # Extract text. .get_text() gets all text from within the tag.
                    # 'separator="\n"' adds newlines between different text blocks for better readability.
                    # 'strip=True' removes leading/trailing whitespace from each line.
                    text_content = content_area.get_text(separator="\n", strip=True)

                    # Write the URL as a header and then the content to the file
                    outfile.write(f"--- Content from: {url} ---\n\n")
                    outfile.write(text_content)
                    outfile.write("\n\n--- End of content from this URL ---\n\n\n")
                    print(f"Successfully scraped and wrote content from {url}")
                else:
                    outfile.write(f"--- Content from: {url} ---\n\n")
                    outfile.write("Could not find the main content area (e.g., div with class 'wiki-content' or id 'main-content') on this page.\n")
                    outfile.write("\n\n--- End of content from this URL ---\n\n\n")
                    print(f"Warning: Could not find main content div for {url}")

            except requests.exceptions.RequestException as e:
                error_message = f"Error scraping {url}: {e}"
                print(error_message)
                outfile.write(f"--- Error for URL: {url} ---\n")
                outfile.write(f"{error_message}\n")
                outfile.write("--- End of error report ---\n\n\n")
            
            print("-" * 30) # Separator in console output

    print(f"\nScraping complete. All content saved to {output_filename}")

if __name__ == "__main__":
    # Before running, make sure you have the required libraries installed:
    # pip install requests beautifulsoup4
    scrape_surf_wiki()