# Python Migration Utilities

Utilities for migrating ABAP code to Python.

## Tools

### 1. `abap_analyzer.py`
Analyzes ABAP code and provides migration insights.

**Features:**
- Extracts data declarations
- Identifies database operations
- Detects function calls and class references
- Assesses migration complexity
- Identifies risks
- Estimates effort
- Provides recommendations

**Usage:**
```python
from abap_analyzer import analyze_abap_code, print_analysis_report

abap_code = """
YOUR ABAP CODE HERE
"""

result = analyze_abap_code(abap_code)
print_analysis_report(result)
```

### 2. `code_generator.py`
Generates Python code templates from ABAP patterns.

**Features:**
- Generate SQLAlchemy models
- Generate Pydantic schemas
- Generate FastAPI CRUD endpoints
- Generate pytest tests
- Generate database configuration
- Generate requirements.txt

**Usage:**
```python
from code_generator import PythonCodeGenerator

# Generate model
fields = {
    'id': 'i',
    'name': 'string',
    'amount': 'p',
}

model_code = PythonCodeGenerator.generate_sqlalchemy_model('customer', fields)
print(model_code)

# Generate API endpoints
api_code = PythonCodeGenerator.generate_fastapi_crud_endpoint('customer', 'Customer')
print(api_code)
```

## Installation

```bash
pip install sqlalchemy pydantic fastapi pytest
```

## Example Workflow

1. **Analyze ABAP code:**
```python
from abap_analyzer import analyze_abap_code
result = analyze_abap_code(your_abap_code)
```

2. **Generate Python code:**
```python
from code_generator import PythonCodeGenerator
model = PythonCodeGenerator.generate_sqlalchemy_model('table_name', fields)
```

3. **Review and customize** the generated code

4. **Add tests** using the test generator

5. **Deploy** your Python application
