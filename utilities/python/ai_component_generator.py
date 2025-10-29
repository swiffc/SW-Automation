#!/usr/bin/env python3
"""
AI-Powered SolidWorks Component Generator
Generates complete C# automation code from natural language descriptions

Usage:
    python ai_component_generator.py "Create a fan assembly with 8 blades"
    python ai_component_generator.py "Generate a support bracket with adjustable height"

Requirements:
    - OpenAI API key in .env file
    - OPENAI_API_KEY=sk-your-key-here
"""

import os
import sys
from pathlib import Path
from datetime import datetime
from openai import OpenAI

class ComponentGenerator:
    """Generate SolidWorks automation C# code using AI"""
    
    def __init__(self):
        """Initialize with OpenAI client"""
        self.api_key = os.getenv('OPENAI_API_KEY')
        if not self.api_key or self.api_key == 'your-openai-api-key-here':
            raise ValueError(
                "OpenAI API key not found!\n"
                "1. Create .env file in project root\n"
                "2. Add your OpenAI API key to .env\n"
                "3. Get key from: https://platform.openai.com/api-keys"
            )
        
        self.client = OpenAI(api_key=self.api_key)
        self.project_root = Path(__file__).parent.parent.parent
        self.template_dir = self.project_root / "macros" / "csharp" / "Solidworks-Automation"
        self.output_dir = self.project_root / "output" / "generated_code"
        
    def load_example_code(self) -> str:
        """Load example component code as context"""
        try:
            # Load Bundle.cs as the primary example
            bundle_file = self.template_dir / "Bundle" / "Bundle.cs"
            if bundle_file.exists():
                return bundle_file.read_text(encoding='utf-8')[:6000]  # First 6000 chars
            else:
                return "// Example code not available"
        except Exception as e:
            print(f"?? Warning: Could not load example code: {e}")
            return "// Example code not available"
    
    def generate_component(self, description: str, component_type: str = "Assembly") -> str:
        """
        Generate complete C# component from natural language
        
        Args:
            description: Natural language description of component
            component_type: Type of component (Assembly, Part, etc.)
        
        Returns:
            Generated C# code
        """
        
        print(f"?? Loading example code...")
        example_code = self.load_example_code()
        
        print(f"?? Analyzing requirements...")
        
        prompt = f"""You are an expert SolidWorks API developer creating C# automation code.

REQUIREMENTS:
Create a complete C# class for: {description}

EXAMPLE PATTERN (Bundle automation):
{example_code}

MUST INCLUDE:
1. Proper namespace: MyNamespace.ComponentName
2. Using statements: System, SolidWorks.Interop.sldworks, FileTools.Infrastructure
3. GlobalErrorHandler for all error logging
4. ComObjectManager for COM safety (prevents memory leaks)
5. Constructor that accepts ModelDoc2 parameter
6. Configuration properties class
7. Main generation/assembly method
8. Proper SolidWorks API calls
9. XML documentation comments
10. Try-catch blocks with GlobalErrorHandler.LogError(ex, context)

CODE STANDARDS:
- Classes: PascalCase
- Methods: PascalCase  
- Private fields: _camelCase (underscore prefix)
- Dispose COM objects properly
- Log all operations with GlobalErrorHandler.LogInfo()
- Add null checks before accessing properties

OUTPUT:
Complete, production-ready C# code that follows the example pattern.
Only output the C# code, no explanations.
"""

        print(f"?? Generating code with GPT-4...")
        
        try:
            response = self.client.chat.completions.create(
                model="gpt-4",
                messages=[
                    {
                        "role": "system", 
                        "content": "You are an expert SolidWorks API automation developer. Generate production-quality C# code."
                    },
                    {
                        "role": "user", 
                        "content": prompt
                    }
                ],
                temperature=0.3,  # Lower temperature for more consistent code
                max_tokens=4000
            )
            
            generated_code = response.choices[0].message.content
            
            # Clean up markdown code blocks if present
            if "```csharp" in generated_code:
                generated_code = generated_code.split("```csharp")[1].split("```")[0].strip()
            elif "```" in generated_code:
                generated_code = generated_code.split("```")[1].split("```")[0].strip()
            
            return generated_code
            
        except Exception as e:
            raise Exception(f"? Error calling OpenAI API: {e}")
    
    def save_generated_code(self, code: str, description: str) -> Path:
        """Save generated code to file"""
        
        # Create output directory
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
        # Generate filename from description
        safe_name = "".join(c if c.isalnum() or c.isspace() else "_" for c in description)
        safe_name = "_".join(safe_name.split())[:50]  # Limit length
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"{safe_name}_{timestamp}.cs"
        
        output_file = self.output_dir / filename
        output_file.write_text(code, encoding='utf-8')
        
        return output_file
    
    def generate_and_save(self, description: str):
        """Main workflow: generate and save component"""
        
        print(f"\n{'='*60}")
        print(f"?? AI COMPONENT GENERATOR")
        print(f"{'='*60}\n")
        print(f"?? Description: {description}\n")
        
        # Generate code
        code = self.generate_component(description)
        
        # Display generated code
        print(f"\n{'='*60}")
        print(f"? GENERATED CODE:")
        print(f"{'='*60}\n")
        print(code)
        
        # Save to file
        output_file = self.save_generated_code(code, description)
        
        print(f"\n{'='*60}")
        print(f"? SAVED TO: {output_file.relative_to(self.project_root)}")
        print(f"{'='*60}\n")
        
        # Next steps
        print("?? NEXT STEPS:")
        print("  1. Review generated code")
        print("  2. Add to your Visual Studio project")
        print("  3. Build and test")
        print("  4. Adjust as needed\n")
        
        return output_file


def main():
    """Main entry point"""
    
    if len(sys.argv) < 2:
        print("? Usage: python ai_component_generator.py 'component description'")
        print("\nExamples:")
        print('  python ai_component_generator.py "Create a fan assembly with 8 blades"')
        print('  python ai_component_generator.py "Generate a support bracket with adjustable height"')
        print('  python ai_component_generator.py "Build a tube bundle with 100 tubes arranged in 10 rows"\n')
        sys.exit(1)
    
    description = sys.argv[1]
    
    try:
        generator = ComponentGenerator()
        generator.generate_and_save(description)
        
    except ValueError as e:
        print(f"\n? Configuration Error:\n{e}\n")
        sys.exit(1)
        
    except Exception as e:
        print(f"\n? Error: {e}\n")
        sys.exit(1)


if __name__ == "__main__":
    main()

