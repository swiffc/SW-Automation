#!/usr/bin/env python3
"""
AI-Powered Test Case Generator
Generates comprehensive unit and integration tests

Usage:
    python ai_test_generator.py --component Bundle
    python ai_test_generator.py --file "SolidWorksService.cs"
    python ai_test_generator.py --all

Requirements:
    - OpenAI API key in .env
"""

import os
import sys
import argparse
from pathlib import Path
from openai import OpenAI

class TestGenerator:
    """Generate comprehensive test cases using AI"""
    
    def __init__(self):
        self.api_key = os.getenv('OPENAI_API_KEY')
        if not self.api_key:
            raise ValueError("OPENAI_API_KEY not found in .env")
        self.client = OpenAI(api_key=self.api_key)
        self.project_root = Path(__file__).parent.parent.parent
        self.code_dir = self.project_root / "macros" / "csharp" / "Solidworks-Automation"
        self.tests_dir = self.project_root / "macros" / "csharp" / "Solidworks-Automation" / "Tests"
    
    def generate_tests_for_file(self, file_path: Path) -> str:
        """Generate comprehensive test cases for a C# file"""
        
        print(f"?? Reading {file_path.name}...")
        code = file_path.read_text(encoding='utf-8')
        
        print(f"?? Generating tests with AI...")
        
        prompt = f"""Generate comprehensive C# test cases for this code:

CODE TO TEST:
{code[:8000]}

REQUIREMENTS:
1. Use NUnit framework
2. Test all public methods
3. Include these test categories:
   - Happy path scenarios
   - Edge cases (null inputs, empty collections, invalid data)
   - Error handling (exceptions thrown correctly)
   - COM object cleanup (if applicable)
   - Integration scenarios

4. For SolidWorks API code:
   - Mock SolidWorks COM objects
   - Test COM safety (proper disposal)
   - Test GlobalErrorHandler integration

5. Test naming: MethodName_Scenario_ExpectedResult

6. Include:
   - [SetUp] and [TearDown] methods
   - Arrange-Act-Assert pattern
   - Clear test documentation

EXAMPLE FORMAT:
```csharp
using NUnit.Framework;
using Moq;
using System;

namespace ComponentTests
{{
    [TestFixture]
    public class ClassNameTests
    {{
        [SetUp]
        public void SetUp()
        {{
            // Test setup
        }}
        
        [Test]
        public void MethodName_ValidInput_ReturnsExpectedResult()
        {{
            // Arrange
            var sut = new ClassName();
            
            // Act
            var result = sut.MethodName(validInput);
            
            // Assert
            Assert.That(result, Is.Not.Null);
        }}
        
        [Test]
        public void MethodName_NullInput_ThrowsArgumentNullException()
        {{
            // Arrange, Act, Assert
        }}
    }}
}}
```

Output complete, production-ready test code.
"""
        
        response = self.client.chat.completions.create(
            model="gpt-4",
            messages=[
                {"role": "system", "content": "You are an expert at writing comprehensive C# unit tests using NUnit."},
                {"role": "user", "content": prompt}
            ],
            temperature=0.3,
            max_tokens=4000
        )
        
        test_code = response.choices[0].message.content
        
        # Clean up markdown if present
        if "```csharp" in test_code:
            test_code = test_code.split("```csharp")[1].split("```")[0].strip()
        elif "```" in test_code:
            test_code = test_code.split("```")[1].split("```")[0].strip()
        
        return test_code
    
    def generate_tests_for_component(self, component_name: str) -> dict:
        """Generate tests for an entire component"""
        
        component_dir = self.code_dir / component_name
        if not component_dir.exists():
            raise FileNotFoundError(f"Component not found: {component_name}")
        
        print(f"\n{'='*60}")
        print(f"?? GENERATING TESTS FOR: {component_name}")
        print(f"{'='*60}\n")
        
        tests = {}
        cs_files = list(component_dir.rglob("*.cs"))[:5]  # Limit to 5 files
        
        for cs_file in cs_files:
            try:
                print(f"\n?? Processing {cs_file.name}...")
                test_code = self.generate_tests_for_file(cs_file)
                tests[cs_file.name] = test_code
                print(f"? Tests generated for {cs_file.name}")
            except Exception as e:
                print(f"? Failed for {cs_file.name}: {e}")
        
        return tests
    
    def save_tests(self, tests: dict, component_name: str):
        """Save generated tests to files"""
        
        component_tests_dir = self.tests_dir / f"{component_name}Tests"
        component_tests_dir.mkdir(parents=True, exist_ok=True)
        
        print(f"\n?? Saving tests to {component_tests_dir}...")
        
        for filename, test_code in tests.items():
            test_filename = filename.replace(".cs", "Tests.cs")
            test_file = component_tests_dir / test_filename
            test_file.write_text(test_code, encoding='utf-8')
            print(f"? Saved: {test_filename}")
        
        # Generate test project file if needed
        self.generate_test_project(component_name)
    
    def generate_test_project(self, component_name: str):
        """Generate .csproj file for tests"""
        
        csproj_content = f"""<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net48</TargetFramework>
    <IsPackable>false</IsPackable>
  </PropertyGroup>

  <ItemGroup>
    <PackageReference Include="NUnit" Version="3.13.3" />
    <PackageReference Include="NUnit3TestAdapter" Version="4.3.1" />
    <PackageReference Include="Microsoft.NET.Test.Sdk" Version="17.4.1" />
    <PackageReference Include="Moq" Version="4.18.4" />
  </ItemGroup>

  <ItemGroup>
    <ProjectReference Include="..\\{component_name}\\{component_name}.csproj" />
    <ProjectReference Include="..\\FileTools\\FileTools.csproj" />
  </ItemGroup>
</Project>
"""
        
        csproj_file = self.tests_dir / f"{component_name}Tests" / f"{component_name}Tests.csproj"
        if not csproj_file.exists():
            csproj_file.write_text(csproj_content, encoding='utf-8')
            print(f"? Created: {csproj_file.name}")


def main():
    parser = argparse.ArgumentParser(description='AI-powered test case generator')
    parser.add_argument('--component', help='Component name (Bundle, Header, etc.)')
    parser.add_argument('--file', help='Specific C# file to generate tests for')
    parser.add_argument('--all', action='store_true', help='Generate tests for all components')
    
    args = parser.parse_args()
    
    try:
        generator = TestGenerator()
        
        if args.all:
            components = ["Bundle", "Header", "Hood", "UnifiedUI"]
            for component in components:
                try:
                    tests = generator.generate_tests_for_component(component)
                    generator.save_tests(tests, component)
                except Exception as e:
                    print(f"? Failed for {component}: {e}")
        
        elif args.component:
            tests = generator.generate_tests_for_component(args.component)
            generator.save_tests(tests, args.component)
            
            print(f"\n{'='*60}")
            print(f"? TEST GENERATION COMPLETE")
            print(f"{'='*60}\n")
            print(f"Location: Tests/{args.component}Tests/")
            print(f"\nNext steps:")
            print(f"  1. Add to Visual Studio solution")
            print(f"  2. Build the test project")
            print(f"  3. Run tests: dotnet test")
            print()
        
        elif args.file:
            file_path = Path(args.file)
            if not file_path.exists():
                print(f"? File not found: {args.file}")
                sys.exit(1)
            
            test_code = generator.generate_tests_for_file(file_path)
            print(f"\n{'='*60}")
            print(f"GENERATED TESTS:")
            print(f"{'='*60}\n")
            print(test_code)
        
        else:
            print("Usage:")
            print("  python ai_test_generator.py --component Bundle")
            print("  python ai_test_generator.py --file 'path/to/file.cs'")
            print("  python ai_test_generator.py --all")
    
    except Exception as e:
        print(f"\n? Error: {e}\n")
        sys.exit(1)


if __name__ == "__main__":
    main()

