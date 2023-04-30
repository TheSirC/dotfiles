#!/usr/local/bin/python
# # -*- coding: utf-8 -*-
import doc2text

# Initialize the class.
doc = doc2text.Document()

# You can pass the lang (as 3 letters code) to the class to improve accuracy
# On ubuntu it requires the package tesseract-ocr-$lang$
# On other OS, see https://github.com/tesseract-ocr/langdata
doc = doc2text.Document(lang="fra")

# Read the file in. Currently accepts pdf, png, jpg, bmp, tiff.
# If reading a PDF, doc2text will split the PDF into its component pages.
doc.read('./Cours.pdf')

# Crop the pages down to estimated text regions, deskew, and optimize for OCR.
doc.process()

# Extract text from the pages.
doc.extract_text()
text = doc.get_text()
