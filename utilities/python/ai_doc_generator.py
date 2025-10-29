#!/usr/bin/env python3
"""
AI-Powered Documentation Generator
Generates comprehensive documentation from code

Usage:
    python ai_doc_generator.py --component Bundle
    python ai_doc_generator.py --file "path/to/Component.cs"
    python ai_doc_generator.py --all

Requirements:
    - OpenAI API key in .env
"""

import os
import sys
import argparse
from pathlib import Path
from datetime import datetime
from openai import OpenAI

class DocGenerator:
    """Generate comprehensive documentation using AI"""
    
    def __init__(self):
        self.api_key = os.getenv('OPENAI_API_KEY')
        if not self.api_key:
            raise ValueError("OPENAI_API_KEY not found in .env")
        self.client = OpenAI(api_key=self.api_key)
        self.project_root = Path(__file__).parent.parent.parent
        self.code_dir = self.project_root / "macros" / "csharp" / "Solidworks-Automation"
        self.docs_dir = self.project_root / "docs" / "Integration"
    
    def get_component_files(self, component_name: str) -> dict:
        """Get all code files for a component"""
        
        component_dir = self.code_dir / component_name
        if not component_dir.exists():
            raise FileNotFoundError(f"Component directory not found: {component_dir}")
        
        files = {}
        for cs_file in component_dir.rglob("*.cs"):
            rel_path = cs_file.relative_to(component_dir)
            try:
                files[str(rel_path)] = cs_file.read_text(encoding='utf-8')[:5000]  # Limit size
            except Exception as e:
                print(f"??  Could not read {cs_file}: {e}")
        
        return files
    
    def generate_integration_guide(self, component_name: str) -> str:
        """Generate complete integration guide for a component"""
        
        print(f"?? Loading code files for {component_name}...")
        files = self.get_component_files(component_name)
        
        # Combine code samples
        code_samples = "\n\n".join([
            f"// {filename}\n{content[:2000]}" 
            for filename, content in list(files.items())[:5]  # First 5 files
        ])
        
        print(f"?? Generating documentation with AI...")
        
        prompt = f"""Generate a comprehensive integration guide for this SolidWorks automation component.

COMPONENT: {component_name}

CODE SAMPLES:
{code_samples}

REQUIRED SECTIONS:

# {component_name.upper()} INTEGRATION GUIDE

## 1. Overview & Purpose
- What does this component do?
- What CAD assemblies/parts does it generate?
- When should engineers use this?

## 2. Technical Architecture
- Key classes and their responsibilities
- Design patterns used (MVVM, Strategy, etc.)
- Dependencies (FileTools, Excel, etc.)
- Data flow diagram (text-based)

## 3. Configuration Parameters
- All configurable properties
- Data types and valid ranges
- Required vs optional parameters
- Default values

## 4. Usage Instructions
### 4.1 Basic Usage
Step-by-step instructions with code examples

### 4.2 Advanced Features
Optional features and their usage

### 4.3 Integration with UnifiedUI
How this integrates with the modern UI

## 5. Code Examples
### Example 1: Simple Generation
```csharp
// Complete working example
```

### Example 2: Advanced Configuration
```csharp
// Advanced example with all features
```

## 6. Testing Procedures
- How to verify the component works
- Expected outputs
- Common test scenarios

## 7. Common Issues & Solutions
- Typical problems users face
- Diagnostic steps
- Solutions and workarounds

## 8. API Reference
- Public methods documentation
- Properties and their descriptions
- Events (if any)

Make the guide practical, accurate, and easy to follow. Use markdown format.
Include real file paths and class names from the code.
"""
        
        response = self.client.chat.completions.create(
            model="gpt-4",
            messages=[
                {"role": "system", "content": "You are a technical documentation expert specializing in SolidWorks API automation."},
                {"role": "user", "content": prompt}
            ],
            temperature=0.3,
            max_tokens=4000
        )
        
        return response.choices[0].message.content
    
    def generate_api_reference(self, file_path: Path) -> str:
        """Generate API reference documentation for a single file"""
        
        code = file_path.read_text(encoding='utf-8')
        
        prompt = f"""Generate API reference documentation for this C# file:

{code[:8000]}

Format as markdown with:
- Class overview
- Public methods (signature, parameters, return value, description)
- Public properties
- Events
- Usage examples

Be concise but complete.
"""
        
        response = self.client.chat.completions.create(
            model="gpt-4",
            messages=[
                {"role": "system", "content": "Generate clear, concise API documentation."},
                {"role": "user", "content": prompt}
            ],
            temperature=0.3,
            max_tokens=3000
        )
        
        return response.choices[0].message.content
    
    def save_documentation(self, content: str, component_name: str, doc_type: str = "INTEGRATION") -> Path:
        """Save generated documentation"""
        
        self.docs_dir.mkdir(parents=True, exist_ok=True)
        
        filename = f"{component_name.upper()}_{doc_type}_GUIDE.md"
        output_file = self.docs_dir / filename
        
        # Add header
        header = f"""# {component_name} {doc_type} Guide

**Auto-generated by AI Documentation Generator**  
**Date**: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}  
**Component**: {component_name}

---

"""
        
        full_content = header + content
        output_file.write_text(full_content, encoding='utf-8')
        
        return output_file
    
    def generate_all_docs(self):
        """Generate documentation for all components"""
        
        components = ["Bundle", "Header", "Hood", "MachineryMount", "Plenum", "Structure", "Walkway"]
        
        print(f"\n{'='*60}")
        print(f"?? GENERATING DOCUMENTATION FOR ALL COMPONENTS")
        print(f"{'='*60}\n")
        
        results = []
        for component in components:
            try:
                print(f"\n?? Processing {component}...")
                content = self.generate_integration_guide(component)
                output_file = self.save_documentation(content, component)
                results.append((component, output_file, "?"))
                print(f"? Saved: {output_file.relative_to(self.project_root)}")
            except Exception as e:
                results.append((component, None, f"? {str(e)}"))
                print(f"? Failed: {e}")
        
        print(f"\n{'='*60}")
        print(f"?? SUMMARY")
        print(f"{'='*60}\n")
        for component, path, status in results:
            print(f"{status} {component}")
        print()


def main():
    parser = argparse.ArgumentParser(description='AI-powered documentation generator')
    parser.add_argument('--component', help='Component name (Bundle, Header, etc.)')
    parser.add_argument('--file', help='Generate API docs for specific file')
    parser.add_argument('--all', action='store_true', help='Generate docs for all components')
    
    args = parser.parse_args()
    
    try:
        generator = DocGenerator()
        
        if args.all:
            generator.generate_all_docs()
        
        elif args.component:
            print(f"\n{'='*60}")
            print(f"?? GENERATING INTEGRATION GUIDE: {args.component}")
            print(f"{'='*60}\n")
            
            content = generator.generate_integration_guide(args.component)
            output_file = generator.save_documentation(content, args.component)
            
            print(f"\n? Documentation saved to:")
            print(f"   {output_file.relative_to(generator.project_root)}\n")
        
        elif args.file:
            file_path = Path(args.file)
            if not file_path.exists():
                print(f"? File not found: {args.file}")
                sys.exit(1)
            
            print(f"\n?? Generating API reference for {file_path.name}...\n")
            content = generator.generate_api_reference(file_path)
            print(content)
        
        else:
            print("Usage:")
            print("  python ai_doc_generator.py --component Bundle")
            print("  python ai_doc_generator.py --file 'path/to/Component.cs'")
            print("  python ai_doc_generator.py --all")
    
    except Exception as e:
        print(f"\n? Error: {e}\n")
        sys.exit(1)


if __name__ == "__main__":
    main()

