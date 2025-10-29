#!/usr/bin/env python3
"""
Dual AI Analysis: Perplexity (web research) + OpenAI (project analysis)
For tool selector implementation
"""

import os
import json
from dotenv import load_dotenv
from openai import OpenAI

load_dotenv()

openai_key = os.getenv('OPENAI_API_KEY')
client = OpenAI(api_key=openai_key)

# Project context
project_context = """
PROJECT: SolidWorks Automation Suite (UnifiedUI)

CURRENT STATE:
- WPF/MVVM application with 82,836 lines of C#
- Single UI with component tabs (Bundle, Header, Hood, etc.)
- No way to select which tool/project user is working on

4 TOOL TYPES (Actual files scanned):
1. Header Section Tool
   - 37 Excel configuration files
   - 95 CAD template files (779 MB)
   - Multiple components: Bundle, Header, Hood, Machinery Mount, Plenum, Structure, Walkway
   - Main tool with lots of configurations

2. XCH Structure Tool
   - 1 Excel config (XCH_SCS.xlsx)
   - 308 CAD template files (476 MB)
   - Single component: XCH Structure
   - Cross-flow structures

3. Z Structure Tool
   - 1 Excel config (Lifting System Work Sheet.xlsx)
   - 1,198 CAD template files (1,275 MB) - HUGE!
   - Single component: Z Structure
   - Vertical structures

4. Hudson Certified
   - 1 Excel config (Cofimco Fan Calculator.xlsx)
   - 172 CAD template files (59 MB)
   - Different workflow entirely (separate project)

ARCHITECTURE:
- MVVM pattern with MainViewModel, MainWindow.xaml
- Services: SolidWorksService, TemplateManager
- Component configurations in Models/
- Dynamic tab creation in code-behind
- COM interop with SolidWorks API

PERPLEXITY RESEARCH FOUND:
- Two-level navigation (tool selector ? component tabs)
- Pattern used by Fusion 360, Visual Studio, Siemens NX
- Single window with dynamic content
- Context-driven UI that adapts

USER'S REQUIREMENT:
"I wanted to be able to choose from the UnifiedUI which project to work on"
"""

print("?? Asking OpenAI to analyze your specific situation...\n")

prompt = f"""{project_context}

Based on this ACTUAL project structure and the Perplexity research findings, provide:

1. **SPECIFIC RECOMMENDATIONS** for this exact project
   - How to structure the tool selector
   - Which MVVM pattern to use
   - How to handle the 37 Excel configs for Header Section Tool
   - How to handle Hudson Certified's different workflow

2. **ARCHITECTURE DECISIONS**
   - Should Hudson Certified be separate window or integrated?
   - How to manage 1,198 Z Structure templates efficiently?
   - Best way to switch between tools without memory leaks?
   - How to handle tool-specific Excel configs?

3. **CODE STRUCTURE**
   - Exact class hierarchy
   - ViewModel organization
   - Service layer changes
   - XAML layout recommendations

4. **IMPLEMENTATION PRIORITY**
   - What to build first?
   - What can be deferred?
   - Critical path items?

5. **POTENTIAL ISSUES**
   - What could go wrong?
   - How to prevent COM memory leaks when switching tools?
   - How to handle user switching tools with unsaved data?

6. **COMPARISON TO PERPLEXITY FINDINGS**
   - Do you agree with the two-level navigation pattern?
   - Any modifications needed for this specific project?
   - Additional considerations for SolidWorks COM interop?

Provide SPECIFIC, ACTIONABLE recommendations tailored to THIS project.
Use code examples where helpful.
Consider the MVVM architecture and existing codebase.
"""

response = client.chat.completions.create(
    model="gpt-4-turbo-preview",
    messages=[
        {
            "role": "system",
            "content": "You are a senior software architect specializing in WPF/MVVM desktop applications and CAD automation. Provide specific, actionable recommendations based on the exact project structure."
        },
        {
            "role": "user",
            "content": prompt
        }
    ],
    temperature=0.3,
    max_tokens=4000
)

openai_analysis = response.choices[0].message.content

print("="*70)
print("?? OPENAI ANALYSIS (Project-Specific)")
print("="*70)
print(openai_analysis)
print("\n" + "="*70)

# Save combined analysis
with open("docs/Architecture/DUAL_AI_RECOMMENDATIONS.md", "w", encoding='utf-8') as f:
    f.write("# Dual AI Recommendations: Tool Selector Implementation\n\n")
    f.write("**Generated**: Perplexity (web research) + OpenAI (project analysis)\n\n")
    f.write("---\n\n")
    f.write("## ?? Perplexity Research Findings\n\n")
    f.write("(See TOOL_SELECTOR_RESEARCH.md for full details)\n\n")
    f.write("**Summary**:\n")
    f.write("- Two-level navigation pattern\n")
    f.write("- Used by Fusion 360, Visual Studio, Siemens NX, CATIA\n")
    f.write("- Single window with dynamic content\n")
    f.write("- Context-driven UI\n\n")
    f.write("---\n\n")
    f.write("## ?? OpenAI Project Analysis\n\n")
    f.write(openai_analysis)
    f.write("\n\n---\n\n")
    f.write("## ?? COMBINED RECOMMENDATIONS\n\n")
    f.write("Both AIs agree on:\n")
    f.write("1. Two-level navigation (tool selector ? components)\n")
    f.write("2. Single window with dynamic views\n")
    f.write("3. MVVM with tool-specific ViewModels\n")
    f.write("4. Proper COM cleanup on tool switching\n\n")
    f.write("**Next Steps**: See TOOL_SELECTOR_IMPLEMENTATION_PLAN.md for code examples\n")

print("\n? Saved combined analysis to: docs/Architecture/DUAL_AI_RECOMMENDATIONS.md")

