#!/usr/bin/env python3
"""
Quick research: Best tool selector patterns for multi-project CAD UI
"""

import os
import requests
from dotenv import load_dotenv

load_dotenv()

perplexity_key = os.getenv('PERPLEXITY_API_KEY')
url = "https://api.perplexity.ai/chat/completions"

headers = {
    "Authorization": f"Bearer {perplexity_key}",
    "Content-Type": "application/json"
}

# Research query
query = """
I'm building a WPF/MVVM application for SolidWorks automation that needs to support 4 different tool types:
1. Header Section Tool (main tool with 37 Excel configurations)
2. XCH Structure Tool (cross-flow structures, Excel-driven)
3. Z Structure Tool (vertical structures, Excel-driven)
4. Hudson Certified (separate project, different workflow)

Each tool has:
- Its own template library (100-1,200+ CAD files)
- Excel-based configuration
- Different components/parts
- Different workflows

Currently, the UI has tabs for individual components (Bundle, Header, Hood, etc.), but users need to:
1. First SELECT which tool they're working with
2. Then configure that tool's components

What are the BEST UI/UX patterns for:
1. Tool/Project selector at the top level?
2. How to organize tabs and navigation?
3. Should tools be separate windows or within one UI?
4. How do enterprise CAD applications (Autodesk, Dassault, Siemens) handle multiple projects/tools?

Find real-world examples from:
- Modern WPF applications
- CAD software (Inventor, CATIA, Fusion 360)
- Visual Studio (multiple project types)
- Engineering tools with multiple workflows

Provide specific UI patterns, code examples if available, and best practices with citations.
"""

payload = {
    "model": "sonar",
    "messages": [
        {
            "role": "system",
            "content": "You are a UX expert specializing in CAD applications and WPF desktop applications. Provide specific, actionable UI patterns with examples."
        },
        {
            "role": "user",
            "content": query
        }
    ]
}

print("?? Researching best tool selector patterns...")
print("??  This will take 30-60 seconds...\n")

try:
    response = requests.post(url, json=payload, headers=headers, timeout=60)
    
    if response.status_code == 200:
        result = response.json()
        content = result['choices'][0]['message']['content']
        
        print("="*70)
        print("?? RESEARCH RESULTS")
        print("="*70)
        print(content)
        print("\n" + "="*70)
        
        # Save to file
        with open("docs/Architecture/TOOL_SELECTOR_RESEARCH.md", "w", encoding='utf-8') as f:
            f.write("# Tool Selector UI/UX Research\n\n")
            f.write("**Generated**: Auto-research via Perplexity\n\n")
            f.write("---\n\n")
            f.write(content)
        
        print("\n? Saved to: docs/Architecture/TOOL_SELECTOR_RESEARCH.md")
        
    else:
        print(f"? Error: {response.status_code}")
        print(response.text)

except Exception as e:
    print(f"? Exception: {e}")

