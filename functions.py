#Altered https://stackoverflow.com/questions/68407519/pypdf2-extract-table-of-contents-outlines-and-their-page-number
from typing import Dict

import fitz  # pip install pymupdf

#Get section numbers for pages
def get_bookmarks(filepath: str) -> Dict[int, str]:
    # WARNING! One page can have multiple bookmarks!
    bookmarks = {}
    with fitz.open(filepath) as doc:
        toc = doc.getToC()  # [[lvl, title, page, …], …]
        for level, title, page in toc:
            bookmarks[page] = title
    return toc
