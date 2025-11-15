# JavaScript/TypeScript Migration Utilities

Utilities for migrating ABAP code to TypeScript/JavaScript.

## Tools

### 1. `abapAnalyzer.ts`
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
```typescript
import { analyzeAbapCode, printAnalysisReport } from './abapAnalyzer';

const abapCode = `
YOUR ABAP CODE HERE
`;

const result = analyzeAbapCode(abapCode);
printAnalysisReport(result);
```

### 2. `codeGenerator.ts`
Generates TypeScript code templates from ABAP patterns.

**Features:**
- Generate Prisma schemas
- Generate TypeORM entities
- Generate Zod validation schemas
- Generate NestJS controllers and services
- Generate package.json
- Support for both Prisma and TypeORM

**Usage:**
```typescript
import { TypeScriptCodeGenerator } from './codeGenerator';

// Generate Prisma schema
const fields = {
  'customer_id': 'n',
  'name': 'string',
  'amount': 'p',
};

const schema = TypeScriptCodeGenerator.generatePrismaSchema('customer', fields);
console.log(schema);

// Generate NestJS controller
const controller = TypeScriptCodeGenerator.generateNestJSController('customer', 'CustomerService');
console.log(controller);

// Generate service
const service = TypeScriptCodeGenerator.generateNestJSService('customer', true);
console.log(service);
```

## Installation

```bash
npm install
```

## Development

```bash
# Build TypeScript
npm run build

# Run analyzer
npm run analyze

# Run code generator
npm run generate

# Run tests
npm test

# Lint code
npm run lint

# Format code
npm run format
```

## Example Workflow

1. **Analyze ABAP code:**
```typescript
import { analyzeAbapCode } from './abapAnalyzer';
const result = analyzeAbapCode(yourAbapCode);
console.log(result);
```

2. **Generate TypeScript code:**
```typescript
import { TypeScriptCodeGenerator } from './codeGenerator';
const entity = TypeScriptCodeGenerator.generateTypeORMEntity('table_name', fields);
const controller = TypeScriptCodeGenerator.generateNestJSController('table_name', 'Service');
```

3. **Review and customize** the generated code

4. **Add tests** and implement business logic

5. **Deploy** your NestJS application

## Recommended Stack

### For Enterprise Applications
- **Framework**: NestJS (provides structure similar to SAP modules)
- **ORM**: Prisma (modern, type-safe) or TypeORM (comprehensive)
- **Validation**: Zod (runtime validation with TypeScript inference)
- **Testing**: Jest
- **API Docs**: Swagger/OpenAPI (built into NestJS)
- **Database**: PostgreSQL or SAP HANA

### For Microservices
- **Framework**: Express + TypeScript or Fastify
- **API**: tRPC (end-to-end type safety)
- **Validation**: Zod
- **ORM**: Prisma
- **Testing**: Vitest

## Migration Patterns

### ABAP Function Module → NestJS Service

**ABAP:**
```abap
FUNCTION z_get_customer.
  IMPORTING iv_customer_id TYPE kunnr
  EXPORTING es_customer TYPE ty_customer
  EXCEPTIONS customer_not_found.
ENDFUNCTION.
```

**TypeScript (NestJS):**
```typescript
@Injectable()
export class CustomerService {
  async getCustomer(customerId: string): Promise<Customer> {
    const customer = await this.prisma.customer.findUnique({
      where: { customerId }
    });

    if (!customer) {
      throw new NotFoundException('Customer not found');
    }

    return customer;
  }
}
```

### ABAP Internal Table → TypeScript Array Operations

**ABAP:**
```abap
DATA lt_items TYPE TABLE OF ty_item.
SELECT * FROM items INTO TABLE @lt_items.
lt_filtered = FILTER #( lt_items WHERE price > 100 ).
SORT lt_filtered BY price DESCENDING.
```

**TypeScript:**
```typescript
const items = await prisma.item.findMany();
const filtered = items
  .filter(item => item.price > 100)
  .sort((a, b) => b.price - a.price);
```

### ABAP SELECT → Prisma Query

**ABAP:**
```abap
SELECT * FROM customers
  INTO TABLE @DATA(lt_customers)
  WHERE country = 'US'
  ORDER BY name.
```

**TypeScript (Prisma):**
```typescript
const customers = await prisma.customer.findMany({
  where: { country: 'US' },
  orderBy: { name: 'asc' }
});
```

## License

Apache-2.0
