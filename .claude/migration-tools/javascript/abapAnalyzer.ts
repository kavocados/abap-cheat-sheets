/**
 * ABAP Code Analyzer for Migration to JavaScript/TypeScript
 *
 * Analyzes ABAP code and extracts information useful for migration
 */

export enum ComplexityLevel {
  SIMPLE = 'Simple',
  MEDIUM = 'Medium',
  COMPLEX = 'Complex',
  VERY_COMPLEX = 'Very Complex'
}

export interface DataDeclaration {
  name: string;
  abapType: string;
  isTable: boolean;
  isStructure: boolean;
  typeScriptEquivalent: string;
}

export interface DatabaseOperation {
  operation: 'SELECT' | 'INSERT' | 'UPDATE' | 'DELETE' | 'MODIFY';
  table: string;
  fields: string[];
  conditions: string[];
  lineNumber: number;
}

export interface FunctionModule {
  name: string;
  importing: DataDeclaration[];
  exporting: DataDeclaration[];
  tables: DataDeclaration[];
  exceptions: string[];
}

export interface AnalysisResult {
  complexity: ComplexityLevel;
  dataDeclarations: DataDeclaration[];
  databaseOperations: DatabaseOperation[];
  functionCalls: string[];
  classReferences: string[];
  risks: string[];
  recommendations: string[];
  estimatedEffortHours: number;
}

export class ABAPAnalyzer {
  private code: string;
  private lines: string[];

  // ABAP to TypeScript type mappings
  private static readonly TYPE_MAPPING: Record<string, string> = {
    'i': 'number',
    'int8': 'bigint',
    'p': 'number',  // Use Decimal library for precision
    'f': 'number',
    'string': 'string',
    'c': 'string',
    'n': 'string',
    'd': 'Date',
    't': 'Date',
    'x': 'Buffer',
    'xstring': 'Buffer',
  };

  // DDIC type mappings
  private static readonly DDIC_MAPPING: Record<string, string> = {
    'DATS': 'Date',
    'TIMS': 'Date',
    'NUMC': 'string',
    'CURR': 'number',
    'QUAN': 'number',
    'CLNT': 'string',
    'LANG': 'string',
    'CUKY': 'string',
    'UNIT': 'string',
  };

  constructor(abapCode: string) {
    this.code = abapCode;
    this.lines = abapCode.split('\n');
  }

  public analyze(): AnalysisResult {
    const dataDeclarations = this.extractDataDeclarations();
    const databaseOperations = this.extractDatabaseOperations();
    const functionCalls = this.extractFunctionCalls();
    const classReferences = this.extractClassReferences();

    const complexity = this.assessComplexity(
      dataDeclarations,
      databaseOperations,
      functionCalls
    );

    const risks = this.identifyRisks();
    const recommendations = this.generateRecommendations(complexity);
    const effort = this.estimateEffort(complexity, databaseOperations, functionCalls);

    return {
      complexity,
      dataDeclarations,
      databaseOperations,
      functionCalls,
      classReferences,
      risks,
      recommendations,
      estimatedEffortHours: effort
    };
  }

  private extractDataDeclarations(): DataDeclaration[] {
    const declarations: DataDeclaration[] = [];
    const dataPattern = /DATA[:\s]+(\w+)\s+TYPE\s+(?:TABLE\s+OF\s+)?(\w+)/i;

    for (const line of this.lines) {
      const lineUpper = line.toUpperCase();
      if (lineUpper.includes('DATA') && lineUpper.includes('TYPE')) {
        const match = lineUpper.match(dataPattern);
        if (match) {
          const varName = match[1].toLowerCase();
          const abapType = match[2].toLowerCase();
          const isTable = lineUpper.includes('TABLE OF');

          const tsType = this.mapToTypeScriptType(abapType, isTable);

          declarations.push({
            name: varName,
            abapType,
            isTable,
            isStructure: false,
            typeScriptEquivalent: tsType
          });
        }
      }
    }

    return declarations;
  }

  private mapToTypeScriptType(abapType: string, isTable: boolean): string {
    const baseType = ABAPAnalyzer.TYPE_MAPPING[abapType] ||
                     ABAPAnalyzer.DDIC_MAPPING[abapType] ||
                     'any';

    return isTable ? `${baseType}[]` : baseType;
  }

  private extractDatabaseOperations(): DatabaseOperation[] {
    const operations: DatabaseOperation[] = [];
    const dbKeywords = ['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'MODIFY'];

    this.lines.forEach((line, index) => {
      const lineUpper = line.toUpperCase().trim();

      for (const keyword of dbKeywords) {
        if (lineUpper.startsWith(keyword)) {
          // Extract table name
          const tableMatch = lineUpper.match(/FROM\s+(@?\w+)/);
          const table = tableMatch ? tableMatch[1].replace('@', '') : 'unknown';

          // Extract fields
          const fields: string[] = [];
          if (keyword === 'SELECT') {
            const fieldsMatch = lineUpper.match(/SELECT\s+(.*?)\s+FROM/);
            if (fieldsMatch && fieldsMatch[1] !== '*') {
              fields.push(...fieldsMatch[1].split(',').map(f => f.trim()));
            }
          }

          // Extract WHERE conditions
          const conditions: string[] = [];
          if (lineUpper.includes('WHERE')) {
            const whereMatch = lineUpper.match(/WHERE\s+(.*?)(?:\.|$)/);
            if (whereMatch) {
              conditions.push(whereMatch[1]);
            }
          }

          operations.push({
            operation: keyword as any,
            table,
            fields,
            conditions,
            lineNumber: index + 1
          });
        }
      }
    });

    return operations;
  }

  private extractFunctionCalls(): string[] {
    const functionCalls: string[] = [];

    for (const line of this.lines) {
      // CALL FUNCTION
      if (line.toUpperCase().includes('CALL FUNCTION')) {
        const match = line.match(/CALL FUNCTION\s+'([^']+)'/i);
        if (match) {
          functionCalls.push(match[1]);
        }
      }

      // Method calls
      if (line.includes('->')) {
        const match = line.match(/(\w+)->(\w+)/);
        if (match) {
          functionCalls.push(`${match[1]}.${match[2]}`);
        }
      }
    }

    return functionCalls;
  }

  private extractClassReferences(): string[] {
    const classes = new Set<string>();

    for (const line of this.lines) {
      // CREATE OBJECT
      if (line.toUpperCase().includes('CREATE OBJECT')) {
        const match = line.match(/TYPE\s+(\w+)/i);
        if (match) {
          classes.add(match[1]);
        }
      }

      // NEW operator
      if (line.toUpperCase().includes('NEW')) {
        const match = line.match(/NEW\s+(\w+)/i);
        if (match) {
          classes.add(match[1]);
        }
      }
    }

    return Array.from(classes);
  }

  private assessComplexity(
    declarations: DataDeclaration[],
    dbOps: DatabaseOperation[],
    functionCalls: string[]
  ): ComplexityLevel {
    let score = 0;

    // Data complexity
    if (declarations.length > 20) score += 2;
    else if (declarations.length > 10) score += 1;

    // Database complexity
    if (dbOps.length > 10) score += 2;
    else if (dbOps.length > 5) score += 1;

    // Integration complexity
    if (functionCalls.length > 15) score += 2;
    else if (functionCalls.length > 5) score += 1;

    // Special patterns
    const codeUpper = this.code.toUpperCase();
    if (codeUpper.includes('CALL FUNCTION') && codeUpper.includes('DESTINATION')) {
      score += 2; // RFC calls
    }

    if (codeUpper.includes('COMMIT WORK') || codeUpper.includes('ROLLBACK WORK')) {
      score += 1; // Transaction handling
    }

    if (codeUpper.includes('AUTHORITY-CHECK')) {
      score += 1; // Authorization
    }

    // Determine level
    if (score === 0) return ComplexityLevel.SIMPLE;
    if (score <= 3) return ComplexityLevel.MEDIUM;
    if (score <= 6) return ComplexityLevel.COMPLEX;
    return ComplexityLevel.VERY_COMPLEX;
  }

  private identifyRisks(): string[] {
    const risks: string[] = [];
    const codeUpper = this.code.toUpperCase();

    if (codeUpper.includes('NATIVE SQL') || codeUpper.includes('EXEC SQL')) {
      risks.push('Native SQL detected - requires database-specific review');
    }

    if (codeUpper.includes('CALL FUNCTION') && codeUpper.includes('DESTINATION')) {
      risks.push('RFC calls detected - requires integration strategy');
    }

    if (codeUpper.includes('AUTHORITY-CHECK')) {
      risks.push('Authorization logic - must map to new security model');
    }

    if (codeUpper.includes('UPDATE TASK') || codeUpper.includes('IN UPDATE TASK')) {
      risks.push('Update tasks - requires async processing design');
    }

    if (codeUpper.includes('COMMIT WORK AND WAIT')) {
      risks.push('Synchronous commits - consider async patterns');
    }

    if (this.databaseOperations.length > 20) {
      risks.push('Heavy database usage - performance testing required');
    }

    return risks;
  }

  private generateRecommendations(complexity: ComplexityLevel): string[] {
    const recommendations: string[] = [];

    recommendations.push(`Complexity Level: ${complexity}`);

    if (complexity === ComplexityLevel.SIMPLE || complexity === ComplexityLevel.MEDIUM) {
      recommendations.push('✓ Good candidate for direct migration');
      recommendations.push('Consider using NestJS for enterprise-grade structure');
      recommendations.push('Use Prisma or TypeORM for database operations');
    } else {
      recommendations.push('⚠ Consider phased migration approach');
      recommendations.push('Extract business logic into service layer');
      recommendations.push('Design API contract before implementation');
    }

    if (this.code.toUpperCase().includes('SELECT')) {
      recommendations.push('Use ORM (Prisma/TypeORM) for database access');
      recommendations.push('Implement connection pooling');
    }

    if (this.code.toUpperCase().includes('CALL FUNCTION')) {
      recommendations.push('Convert function modules to REST/GraphQL APIs');
      recommendations.push('Use NestJS with decorators for clean architecture');
    }

    recommendations.push('Implement comprehensive tests (Jest/Vitest)');
    recommendations.push('Add API documentation (Swagger/OpenAPI)');
    recommendations.push('Set up TypeScript strict mode');
    recommendations.push('Use Zod for runtime validation');

    return recommendations;
  }

  private estimateEffort(
    complexity: ComplexityLevel,
    dbOps: DatabaseOperation[],
    functionCalls: string[]
  ): number {
    const baseHours: Record<ComplexityLevel, number> = {
      [ComplexityLevel.SIMPLE]: 8,
      [ComplexityLevel.MEDIUM]: 24,
      [ComplexityLevel.COMPLEX]: 80,
      [ComplexityLevel.VERY_COMPLEX]: 200
    };

    let hours = baseHours[complexity];

    // Add for database operations
    hours += dbOps.length * 2;

    // Add for function calls
    hours += functionCalls.length * 3;

    // Add for testing and documentation
    hours += hours * 0.3;

    return Math.round(hours);
  }
}

export function analyzeAbapCode(abapCode: string): AnalysisResult {
  const analyzer = new ABAPAnalyzer(abapCode);
  return analyzer.analyze();
}

export function printAnalysisReport(result: AnalysisResult): void {
  console.log('='.repeat(80));
  console.log('ABAP Migration Analysis Report');
  console.log('='.repeat(80));

  console.log(`\n📊 Complexity: ${result.complexity}`);
  console.log(`⏱️  Estimated Effort: ${result.estimatedEffortHours} hours`);

  console.log(`\n📋 Data Declarations (${result.dataDeclarations.length}):`);
  result.dataDeclarations.slice(0, 10).forEach(decl => {
    console.log(`  • ${decl.name}: ${decl.abapType} → ${decl.typeScriptEquivalent}`);
  });
  if (result.dataDeclarations.length > 10) {
    console.log(`  ... and ${result.dataDeclarations.length - 10} more`);
  }

  console.log(`\n💾 Database Operations (${result.databaseOperations.length}):`);
  result.databaseOperations.slice(0, 10).forEach(op => {
    console.log(`  • ${op.operation} on ${op.table} (line ${op.lineNumber})`);
  });
  if (result.databaseOperations.length > 10) {
    console.log(`  ... and ${result.databaseOperations.length - 10} more`);
  }

  console.log(`\n📞 Function Calls (${result.functionCalls.length}):`);
  result.functionCalls.slice(0, 10).forEach(call => {
    console.log(`  • ${call}`);
  });
  if (result.functionCalls.length > 10) {
    console.log(`  ... and ${result.functionCalls.length - 10} more`);
  }

  if (result.risks.length > 0) {
    console.log(`\n⚠️  Risks (${result.risks.length}):`);
    result.risks.forEach(risk => {
      console.log(`  • ${risk}`);
    });
  }

  console.log(`\n💡 Recommendations (${result.recommendations.length}):`);
  result.recommendations.forEach(rec => {
    console.log(`  • ${rec}`);
  });

  console.log('\n' + '='.repeat(80));
}

// Example usage
if (require.main === module) {
  const sampleAbap = `
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
  `;

  const result = analyzeAbapCode(sampleAbap);
  printAnalysisReport(result);
}
