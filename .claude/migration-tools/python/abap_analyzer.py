"""
ABAP Code Analyzer for Migration to Python

This module provides utilities to analyze ABAP code and extract information
useful for migration to Python.
"""

import re
from dataclasses import dataclass
from typing import List, Dict, Set, Optional
from enum import Enum


class ComplexityLevel(Enum):
    """Migration complexity levels"""
    SIMPLE = "Simple"
    MEDIUM = "Medium"
    COMPLEX = "Complex"
    VERY_COMPLEX = "Very Complex"


@dataclass
class DataDeclaration:
    """Represents an ABAP data declaration"""
    name: str
    abap_type: str
    is_table: bool = False
    is_structure: bool = False
    python_equivalent: str = ""


@dataclass
class DatabaseOperation:
    """Represents a database operation"""
    operation: str  # SELECT, INSERT, UPDATE, DELETE, MODIFY
    table: str
    fields: List[str]
    conditions: List[str]
    line_number: int


@dataclass
class FunctionModule:
    """Represents an ABAP function module"""
    name: str
    importing: List[DataDeclaration]
    exporting: List[DataDeclaration]
    tables: List[DataDeclaration]
    exceptions: List[str]


@dataclass
class AnalysisResult:
    """Complete analysis result"""
    complexity: ComplexityLevel
    data_declarations: List[DataDeclaration]
    database_operations: List[DatabaseOperation]
    function_calls: List[str]
    class_references: List[str]
    risks: List[str]
    recommendations: List[str]
    estimated_effort_hours: int


class ABAPAnalyzer:
    """Analyze ABAP code for migration"""

    # ABAP to Python type mappings
    TYPE_MAPPING = {
        'i': 'int',
        'int8': 'int',
        'p': 'Decimal',
        'f': 'float',
        'string': 'str',
        'c': 'str',
        'n': 'str',  # Numeric string
        'd': 'date',
        't': 'time',
        'x': 'bytes',
        'xstring': 'bytes',
    }

    # DDIC type mappings
    DDIC_MAPPING = {
        'DATS': 'date',
        'TIMS': 'time',
        'NUMC': 'str',
        'CURR': 'Decimal',
        'QUAN': 'Decimal',
        'CLNT': 'str',
        'LANG': 'str',
        'CUKY': 'str',
        'UNIT': 'str',
    }

    def __init__(self, abap_code: str):
        self.code = abap_code
        self.lines = abap_code.split('\n')

    def analyze(self) -> AnalysisResult:
        """Perform complete analysis"""
        data_declarations = self._extract_data_declarations()
        db_operations = self._extract_database_operations()
        function_calls = self._extract_function_calls()
        class_references = self._extract_class_references()

        complexity = self._assess_complexity(
            data_declarations, db_operations, function_calls
        )

        risks = self._identify_risks()
        recommendations = self._generate_recommendations(complexity)
        effort = self._estimate_effort(complexity, db_operations, function_calls)

        return AnalysisResult(
            complexity=complexity,
            data_declarations=data_declarations,
            database_operations=db_operations,
            function_calls=function_calls,
            class_references=class_references,
            risks=risks,
            recommendations=recommendations,
            estimated_effort_hours=effort
        )

    def _extract_data_declarations(self) -> List[DataDeclaration]:
        """Extract DATA declarations"""
        declarations = []

        # Pattern for DATA declarations
        data_pattern = r'DATA[:\s]+(\w+)\s+TYPE\s+(?:TABLE\s+OF\s+)?(\w+)'

        for line in self.lines:
            line_upper = line.upper()
            if 'DATA' in line_upper and 'TYPE' in line_upper:
                match = re.search(data_pattern, line_upper)
                if match:
                    var_name = match.group(1).lower()
                    abap_type = match.group(2).lower()
                    is_table = 'TABLE OF' in line_upper

                    # Map to Python type
                    python_type = self._map_to_python_type(abap_type, is_table)

                    declarations.append(DataDeclaration(
                        name=var_name,
                        abap_type=abap_type,
                        is_table=is_table,
                        python_equivalent=python_type
                    ))

        return declarations

    def _map_to_python_type(self, abap_type: str, is_table: bool) -> str:
        """Map ABAP type to Python equivalent"""
        base_type = self.TYPE_MAPPING.get(
            abap_type,
            self.DDIC_MAPPING.get(abap_type, 'Any')
        )

        if is_table:
            return f'List[{base_type}]'

        return base_type

    def _extract_database_operations(self) -> List[DatabaseOperation]:
        """Extract database operations"""
        operations = []

        db_keywords = ['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'MODIFY']

        for i, line in enumerate(self.lines):
            line_upper = line.upper().strip()

            for keyword in db_keywords:
                if line_upper.startswith(keyword):
                    # Extract table name
                    table_match = re.search(r'FROM\s+(@?\w+)', line_upper)
                    table = table_match.group(1).replace('@', '') if table_match else 'unknown'

                    # Extract fields (simplified)
                    fields = []
                    if keyword == 'SELECT':
                        fields_match = re.search(r'SELECT\s+(.*?)\s+FROM', line_upper)
                        if fields_match:
                            fields_str = fields_match.group(1)
                            if fields_str != '*':
                                fields = [f.strip() for f in fields_str.split(',')]

                    # Extract WHERE conditions
                    conditions = []
                    if 'WHERE' in line_upper:
                        where_match = re.search(r'WHERE\s+(.*?)(?:\.|$)', line_upper)
                        if where_match:
                            conditions = [where_match.group(1)]

                    operations.append(DatabaseOperation(
                        operation=keyword,
                        table=table,
                        fields=fields,
                        conditions=conditions,
                        line_number=i + 1
                    ))

        return operations

    def _extract_function_calls(self) -> List[str]:
        """Extract function module calls"""
        function_calls = []

        for line in self.lines:
            # CALL FUNCTION
            if 'CALL FUNCTION' in line.upper():
                match = re.search(r"CALL FUNCTION\s+'([^']+)'", line, re.IGNORECASE)
                if match:
                    function_calls.append(match.group(1))

            # Method calls
            if '->' in line:
                match = re.search(r'(\w+)->(\w+)', line)
                if match:
                    function_calls.append(f"{match.group(1)}.{match.group(2)}")

        return function_calls

    def _extract_class_references(self) -> List[str]:
        """Extract class references"""
        classes = set()

        for line in self.lines:
            # CREATE OBJECT
            if 'CREATE OBJECT' in line.upper():
                match = re.search(r'TYPE\s+(\w+)', line, re.IGNORECASE)
                if match:
                    classes.add(match.group(1))

            # NEW operator
            if 'NEW' in line.upper():
                match = re.search(r'NEW\s+(\w+)', line, re.IGNORECASE)
                if match:
                    classes.add(match.group(1))

        return list(classes)

    def _assess_complexity(
        self,
        declarations: List[DataDeclaration],
        db_ops: List[DatabaseOperation],
        function_calls: List[str]
    ) -> ComplexityLevel:
        """Assess migration complexity"""
        score = 0

        # Data complexity
        if len(declarations) > 20:
            score += 2
        elif len(declarations) > 10:
            score += 1

        # Database complexity
        if len(db_ops) > 10:
            score += 2
        elif len(db_ops) > 5:
            score += 1

        # Integration complexity
        if len(function_calls) > 15:
            score += 2
        elif len(function_calls) > 5:
            score += 1

        # Special patterns
        code_upper = self.code.upper()
        if 'CALL FUNCTION' in code_upper and 'DESTINATION' in code_upper:
            score += 2  # RFC calls

        if 'COMMIT WORK' in code_upper or 'ROLLBACK WORK' in code_upper:
            score += 1  # Transaction handling

        if 'AUTHORITY-CHECK' in code_upper:
            score += 1  # Authorization

        if 'BAPI' in code_upper:
            score += 1  # BAPI usage

        # Determine level
        if score == 0:
            return ComplexityLevel.SIMPLE
        elif score <= 3:
            return ComplexityLevel.MEDIUM
        elif score <= 6:
            return ComplexityLevel.COMPLEX
        else:
            return ComplexityLevel.VERY_COMPLEX

    def _identify_risks(self) -> List[str]:
        """Identify migration risks"""
        risks = []
        code_upper = self.code.upper()

        if 'NATIVE SQL' in code_upper or 'EXEC SQL' in code_upper:
            risks.append("Native SQL detected - requires database-specific review")

        if 'CALL FUNCTION' in code_upper and 'DESTINATION' in code_upper:
            risks.append("RFC calls detected - requires integration strategy")

        if 'AUTHORITY-CHECK' in code_upper:
            risks.append("Authorization logic - must map to new security model")

        if 'UPDATE TASK' in code_upper or 'IN UPDATE TASK' in code_upper:
            risks.append("Update tasks - requires async processing design")

        if 'COMMIT WORK AND WAIT' in code_upper:
            risks.append("Synchronous commits - consider async patterns")

        if 'ENHANCEMENT-POINT' in code_upper or 'BADI' in code_upper:
            risks.append("Enhancement framework - needs plugin architecture")

        if len(self._extract_database_operations()) > 20:
            risks.append("Heavy database usage - performance testing required")

        return risks

    def _generate_recommendations(self, complexity: ComplexityLevel) -> List[str]:
        """Generate migration recommendations"""
        recommendations = []

        recommendations.append(f"Complexity Level: {complexity.value}")

        if complexity in [ComplexityLevel.SIMPLE, ComplexityLevel.MEDIUM]:
            recommendations.append("✓ Good candidate for direct migration")
            recommendations.append("Consider using FastAPI for web services")
            recommendations.append("Use SQLAlchemy for database operations")
        else:
            recommendations.append("⚠ Consider phased migration approach")
            recommendations.append("Extract business logic into service layer")
            recommendations.append("Design API contract before implementation")

        if 'SELECT' in self.code.upper():
            recommendations.append("Use ORM (SQLAlchemy) for database access")
            recommendations.append("Add database connection pooling")

        if 'CALL FUNCTION' in self.code.upper():
            recommendations.append("Convert function modules to REST APIs")
            recommendations.append("Use FastAPI or Flask for HTTP endpoints")

        recommendations.append("Implement comprehensive unit tests (pytest)")
        recommendations.append("Add API documentation (OpenAPI/Swagger)")
        recommendations.append("Set up CI/CD pipeline")

        return recommendations

    def _estimate_effort(
        self,
        complexity: ComplexityLevel,
        db_ops: List[DatabaseOperation],
        function_calls: List[str]
    ) -> int:
        """Estimate migration effort in hours"""
        base_hours = {
            ComplexityLevel.SIMPLE: 8,
            ComplexityLevel.MEDIUM: 24,
            ComplexityLevel.COMPLEX: 80,
            ComplexityLevel.VERY_COMPLEX: 200
        }

        hours = base_hours[complexity]

        # Add for database operations
        hours += len(db_ops) * 2

        # Add for function calls
        hours += len(function_calls) * 3

        # Add for testing and documentation
        hours += hours * 0.3

        return int(hours)


def analyze_abap_code(abap_code: str) -> AnalysisResult:
    """Convenience function to analyze ABAP code"""
    analyzer = ABAPAnalyzer(abap_code)
    return analyzer.analyze()


def print_analysis_report(result: AnalysisResult):
    """Print formatted analysis report"""
    print("=" * 80)
    print("ABAP Migration Analysis Report")
    print("=" * 80)

    print(f"\n📊 Complexity: {result.complexity.value}")
    print(f"⏱️  Estimated Effort: {result.estimated_effort_hours} hours")

    print(f"\n📋 Data Declarations ({len(result.data_declarations)}):")
    for decl in result.data_declarations[:10]:  # Show first 10
        print(f"  • {decl.name}: {decl.abap_type} → {decl.python_equivalent}")
    if len(result.data_declarations) > 10:
        print(f"  ... and {len(result.data_declarations) - 10} more")

    print(f"\n💾 Database Operations ({len(result.database_operations)}):")
    for op in result.database_operations[:10]:
        print(f"  • {op.operation} on {op.table} (line {op.line_number})")
    if len(result.database_operations) > 10:
        print(f"  ... and {len(result.database_operations) - 10} more")

    print(f"\n📞 Function Calls ({len(result.function_calls)}):")
    for call in result.function_calls[:10]:
        print(f"  • {call}")
    if len(result.function_calls) > 10:
        print(f"  ... and {len(result.function_calls) - 10} more")

    if result.risks:
        print(f"\n⚠️  Risks ({len(result.risks)}):")
        for risk in result.risks:
            print(f"  • {risk}")

    print(f"\n💡 Recommendations ({len(result.recommendations)}):")
    for rec in result.recommendations:
        print(f"  • {rec}")

    print("\n" + "=" * 80)


if __name__ == "__main__":
    # Example usage
    sample_abap = """
    REPORT z_test_migration.

    DATA: lt_customers TYPE TABLE OF kna1,
          lv_count TYPE i.

    SELECT * FROM kna1
      INTO TABLE @lt_customers
      WHERE land1 = 'US'.

    lv_count = lines( lt_customers ).

    LOOP AT lt_customers INTO DATA(ls_customer).
      WRITE: / ls_customer-kunnr, ls_customer-name1.
    ENDLOOP.
    """

    result = analyze_abap_code(sample_abap)
    print_analysis_report(result)
