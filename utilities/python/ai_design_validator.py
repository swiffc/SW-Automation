#!/usr/bin/env python3
"""
AI-Powered Design Validator
Validates CAD component designs against engineering rules

Usage:
    python ai_design_validator.py --config bundle_config.json
    python ai_design_validator.py --component Bundle --interactive

Requirements:
    - OpenAI API key in .env
"""

import os
import sys
import json
import argparse
from pathlib import Path
from openai import OpenAI

class DesignValidator:
    """Validate component designs using AI"""
    
    def __init__(self):
        self.api_key = os.getenv('OPENAI_API_KEY')
        if not self.api_key:
            raise ValueError("OPENAI_API_KEY not found in .env")
        self.client = OpenAI(api_key=self.api_key)
    
    def validate_bundle_design(self, config: dict) -> dict:
        """Validate heat exchanger bundle design"""
        
        prompt = f"""You are a heat exchanger design engineer. Review this bundle design:

DESIGN PARAMETERS:
- Width: {config.get('width', 'N/A')} inches
- Depth: {config.get('depth', 'N/A')} inches
- Tube Count: {config.get('tube_count', 'N/A')}
- Tube OD: {config.get('tube_od', 'N/A')} inches
- Tube Wall Thickness: {config.get('tube_wall', 'N/A')} inches
- Vertical Pitch (Front): {config.get('vertical_pitch_front', 'N/A')} inches
- Vertical Pitch (Rear): {config.get('vertical_pitch_rear', 'N/A')} inches
- Horizontal Pitch: {config.get('horizontal_pitch', 'N/A')} inches
- Material: {config.get('material', 'N/A')}
- Operating Temp: {config.get('operating_temp', 'N/A')}°F
- Operating Pressure: {config.get('operating_pressure', 'N/A')} PSI

ENGINEERING RULES TO CHECK:
1. Tube spacing (pitch) must be ? 1.25 × tube OD (prevents tube interference)
2. Bundle dimensions must accommodate tube count with proper spacing
3. Vertical pitch must allow for thermal expansion (~0.5" minimum)
4. Tube wall thickness must be adequate for pressure (min 0.049" for most applications)
5. Material must be suitable for operating temperature
6. Total tube bundle must fit within specified width/depth
7. Horizontal pitch should be 1.5-2.0 × tube OD for optimal airflow
8. Front and rear vertical pitches can differ but should be within 20% of each other

VALIDATION REQUIRED:
1. Does this design pass all engineering rules?
2. If NO, what specific violations exist?
3. What corrections are needed?
4. Are there any warnings or optimization suggestions?

Respond in JSON format:
{{
    "valid": true/false,
    "violations": ["list of rule violations"],
    "warnings": ["list of warnings"],
    "suggestions": ["optimization suggestions"],
    "corrected_values": {{"param": "suggested value"}},
    "overall_assessment": "brief summary"
}}
"""
        
        response = self.client.chat.completions.create(
            model="gpt-4",
            messages=[
                {"role": "system", "content": "You are an expert heat exchanger design engineer. Validate designs against engineering standards."},
                {"role": "user", "content": prompt}
            ],
            temperature=0.2,
            response_format={"type": "json_object"}
        )
        
        return json.loads(response.choices[0].message.content)
    
    def validate_header_design(self, config: dict) -> dict:
        """Validate header design"""
        
        prompt = f"""Validate this header design:

PARAMETERS:
- Type: {config.get('type', 'N/A')}
- Diameter: {config.get('diameter', 'N/A')} inches
- Length: {config.get('length', 'N/A')} inches
- Nozzle Count: {config.get('nozzle_count', 'N/A')}
- Nozzle Size: {config.get('nozzle_size', 'N/A')} inches
- Wall Thickness: {config.get('wall_thickness', 'N/A')} inches
- Material: {config.get('material', 'N/A')}
- Design Pressure: {config.get('design_pressure', 'N/A')} PSI

Check ASME standards and provide validation in JSON format.
"""
        
        response = self.client.chat.completions.create(
            model="gpt-4",
            messages=[
                {"role": "system", "content": "You are an ASME pressure vessel design expert."},
                {"role": "user", "content": prompt}
            ],
            temperature=0.2,
            response_format={"type": "json_object"}
        )
        
        return json.loads(response.choices[0].message.content)
    
    def interactive_validation(self, component_type: str):
        """Interactive design validation"""
        
        print(f"\n{'='*60}")
        print(f"?? AI DESIGN VALIDATOR - {component_type}")
        print(f"{'='*60}\n")
        
        config = {}
        
        if component_type.lower() == "bundle":
            config['width'] = input("Width (inches): ")
            config['depth'] = input("Depth (inches): ")
            config['tube_count'] = input("Tube Count: ")
            config['tube_od'] = input("Tube OD (inches): ")
            config['tube_wall'] = input("Tube Wall Thickness (inches): ")
            config['vertical_pitch_front'] = input("Vertical Pitch Front (inches): ")
            config['vertical_pitch_rear'] = input("Vertical Pitch Rear (inches): ")
            config['horizontal_pitch'] = input("Horizontal Pitch (inches): ")
            config['material'] = input("Material: ")
            config['operating_temp'] = input("Operating Temperature (°F): ")
            config['operating_pressure'] = input("Operating Pressure (PSI): ")
            
            print(f"\n?? Validating design with AI...\n")
            result = self.validate_bundle_design(config)
            
        elif component_type.lower() == "header":
            config['type'] = input("Header Type: ")
            config['diameter'] = input("Diameter (inches): ")
            config['length'] = input("Length (inches): ")
            config['nozzle_count'] = input("Nozzle Count: ")
            config['nozzle_size'] = input("Nozzle Size (inches): ")
            config['wall_thickness'] = input("Wall Thickness (inches): ")
            config['material'] = input("Material: ")
            config['design_pressure'] = input("Design Pressure (PSI): ")
            
            print(f"\n?? Validating design with AI...\n")
            result = self.validate_header_design(config)
        else:
            print(f"? Component type '{component_type}' not yet supported")
            return
        
        self.display_results(result)
    
    def display_results(self, result: dict):
        """Display validation results"""
        
        print(f"{'='*60}")
        if result.get('valid'):
            print("? DESIGN VALIDATION: PASSED")
        else:
            print("? DESIGN VALIDATION: FAILED")
        print(f"{'='*60}\n")
        
        if result.get('violations'):
            print("?? VIOLATIONS:")
            for v in result['violations']:
                print(f"  • {v}")
            print()
        
        if result.get('warnings'):
            print("??  WARNINGS:")
            for w in result['warnings']:
                print(f"  • {w}")
            print()
        
        if result.get('suggestions'):
            print("?? SUGGESTIONS:")
            for s in result['suggestions']:
                print(f"  • {s}")
            print()
        
        if result.get('corrected_values'):
            print("?? RECOMMENDED CORRECTIONS:")
            for param, value in result['corrected_values'].items():
                print(f"  • {param}: {value}")
            print()
        
        if result.get('overall_assessment'):
            print("?? ASSESSMENT:")
            print(f"  {result['overall_assessment']}\n")


def main():
    parser = argparse.ArgumentParser(description='AI-powered design validator')
    parser.add_argument('--config', help='JSON config file to validate')
    parser.add_argument('--component', help='Component type (Bundle, Header, etc.)')
    parser.add_argument('--interactive', action='store_true', help='Interactive mode')
    
    args = parser.parse_args()
    
    try:
        validator = DesignValidator()
        
        if args.interactive and args.component:
            validator.interactive_validation(args.component)
        elif args.config:
            config_file = Path(args.config)
            if not config_file.exists():
                print(f"? Config file not found: {args.config}")
                sys.exit(1)
            
            config = json.loads(config_file.read_text())
            component_type = config.get('type', 'bundle').lower()
            
            if component_type == 'bundle':
                result = validator.validate_bundle_design(config)
            elif component_type == 'header':
                result = validator.validate_header_design(config)
            else:
                print(f"? Unknown component type: {component_type}")
                sys.exit(1)
            
            validator.display_results(result)
        else:
            print("Usage:")
            print("  python ai_design_validator.py --component Bundle --interactive")
            print("  python ai_design_validator.py --config design.json")
    
    except Exception as e:
        print(f"\n? Error: {e}\n")
        sys.exit(1)


if __name__ == "__main__":
    main()

