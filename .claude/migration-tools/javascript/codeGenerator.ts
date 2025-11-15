/**
 * TypeScript/JavaScript Code Generator for ABAP Migration
 *
 * Generates code templates based on ABAP patterns
 */

export interface FieldDefinition {
  name: string;
  type: string;
  required?: boolean;
  unique?: boolean;
}

export class TypeScriptCodeGenerator {
  /**
   * Generate Prisma schema from ABAP table structure
   */
  static generatePrismaSchema(tableName: string, fields: Record<string, string>): string {
    const modelName = this.toPascalCase(tableName);

    let schema = `// Prisma schema for ${tableName}\n`;
    schema += `// Migrated from ABAP table ${tableName.toUpperCase()}\n\n`;
    schema += `model ${modelName} {\n`;
    schema += `  id        Int      @id @default(autoincrement())\n`;

    for (const [fieldName, fieldType] of Object.entries(fields)) {
      const prismaType = this.mapToPrismaType(fieldType);
      schema += `  ${fieldName.toLowerCase()}  ${prismaType}\n`;
    }

    schema += `  createdAt DateTime @default(now())\n`;
    schema += `  updatedAt DateTime @updatedAt\n\n`;
    schema += `  @@map("${tableName.toLowerCase()}")\n`;
    schema += `}\n`;

    return schema;
  }

  /**
   * Generate TypeORM entity from ABAP table structure
   */
  static generateTypeORMEntity(tableName: string, fields: Record<string, string>): string {
    const className = this.toPascalCase(tableName);

    let code = `/**\n * TypeORM entity for ${tableName}\n`;
    code += ` * Migrated from ABAP table ${tableName.toUpperCase()}\n */\n\n`;
    code += `import {\n`;
    code += `  Entity,\n`;
    code += `  Column,\n`;
    code += `  PrimaryGeneratedColumn,\n`;
    code += `  CreateDateColumn,\n`;
    code += `  UpdateDateColumn,\n`;
    code += `} from 'typeorm';\n\n`;

    code += `@Entity('${tableName.toLowerCase()}')\n`;
    code += `export class ${className} {\n`;
    code += `  @PrimaryGeneratedColumn()\n`;
    code += `  id: number;\n\n`;

    for (const [fieldName, fieldType] of Object.entries(fields)) {
      const tsType = this.mapToTypeScriptType(fieldType);
      const columnType = this.mapToTypeORMColumnType(fieldType);

      code += `  @Column(${columnType})\n`;
      code += `  ${fieldName.toLowerCase()}: ${tsType};\n\n`;
    }

    code += `  @CreateDateColumn()\n`;
    code += `  createdAt: Date;\n\n`;
    code += `  @UpdateDateColumn()\n`;
    code += `  updatedAt: Date;\n`;
    code += `}\n`;

    return code;
  }

  /**
   * Generate Zod validation schema
   */
  static generateZodSchema(name: string, fields: Record<string, string>): string {
    const schemaName = this.toPascalCase(name);

    let code = `/**\n * Zod validation schemas for ${name}\n */\n\n`;
    code += `import { z } from 'zod';\n\n`;

    code += `export const ${schemaName}Schema = z.object({\n`;

    for (const [fieldName, fieldType] of Object.entries(fields)) {
      const zodType = this.mapToZodType(fieldType);
      code += `  ${fieldName.toLowerCase()}: ${zodType},\n`;
    }

    code += `});\n\n`;

    code += `export const Create${schemaName}Schema = ${schemaName}Schema.omit({ id: true });\n\n`;
    code += `export const Update${schemaName}Schema = ${schemaName}Schema.partial();\n\n`;

    code += `export type ${schemaName} = z.infer<typeof ${schemaName}Schema>;\n`;
    code += `export type Create${schemaName} = z.infer<typeof Create${schemaName}Schema>;\n`;
    code += `export type Update${schemaName} = z.infer<typeof Update${schemaName}Schema>;\n`;

    return code;
  }

  /**
   * Generate NestJS controller with CRUD endpoints
   */
  static generateNestJSController(entityName: string, serviceName: string): string {
    const className = this.toPascalCase(entityName);
    const routeName = entityName.toLowerCase();

    let code = `/**\n * NestJS Controller for ${entityName}\n`;
    code += ` * Migrated from ABAP function modules\n */\n\n`;
    code += `import {\n`;
    code += `  Controller,\n`;
    code += `  Get,\n`;
    code += `  Post,\n`;
    code += `  Put,\n`;
    code += `  Delete,\n`;
    code += `  Body,\n`;
    code += `  Param,\n`;
    code += `  Query,\n`;
    code += `  ParseIntPipe,\n`;
    code += `  HttpCode,\n`;
    code += `  HttpStatus,\n`;
    code += `} from '@nestjs/common';\n`;
    code += `import { ApiTags, ApiOperation, ApiResponse } from '@nestjs/swagger';\n`;
    code += `import { ${className}Service } from './${routeName}.service';\n`;
    code += `import { Create${className}Dto, Update${className}Dto, ${className}ResponseDto } from './dto';\n\n`;

    code += `@ApiTags('${routeName}')\n`;
    code += `@Controller('${routeName}')\n`;
    code += `export class ${className}Controller {\n`;
    code += `  constructor(private readonly ${this.toCamelCase(className)}Service: ${className}Service) {}\n\n`;

    // GET all
    code += `  @Get()\n`;
    code += `  @ApiOperation({ summary: 'Get all ${routeName}' })\n`;
    code += `  @ApiResponse({ status: 200, description: 'List of ${routeName}', type: [${className}ResponseDto] })\n`;
    code += `  async findAll(\n`;
    code += `    @Query('skip') skip?: number,\n`;
    code += `    @Query('take') take?: number,\n`;
    code += `  ): Promise<${className}ResponseDto[]> {\n`;
    code += `    return this.${this.toCamelCase(className)}Service.findAll({ skip, take });\n`;
    code += `  }\n\n`;

    // GET by id
    code += `  @Get(':id')\n`;
    code += `  @ApiOperation({ summary: 'Get ${routeName} by ID' })\n`;
    code += `  @ApiResponse({ status: 200, description: 'The found ${routeName}', type: ${className}ResponseDto })\n`;
    code += `  @ApiResponse({ status: 404, description: '${className} not found' })\n`;
    code += `  async findOne(@Param('id', ParseIntPipe) id: number): Promise<${className}ResponseDto> {\n`;
    code += `    return this.${this.toCamelCase(className)}Service.findOne(id);\n`;
    code += `  }\n\n`;

    // POST
    code += `  @Post()\n`;
    code += `  @ApiOperation({ summary: 'Create new ${routeName}' })\n`;
    code += `  @ApiResponse({ status: 201, description: 'The created ${routeName}', type: ${className}ResponseDto })\n`;
    code += `  async create(@Body() createDto: Create${className}Dto): Promise<${className}ResponseDto> {\n`;
    code += `    return this.${this.toCamelCase(className)}Service.create(createDto);\n`;
    code += `  }\n\n`;

    // PUT
    code += `  @Put(':id')\n`;
    code += `  @ApiOperation({ summary: 'Update ${routeName}' })\n`;
    code += `  @ApiResponse({ status: 200, description: 'The updated ${routeName}', type: ${className}ResponseDto })\n`;
    code += `  @ApiResponse({ status: 404, description: '${className} not found' })\n`;
    code += `  async update(\n`;
    code += `    @Param('id', ParseIntPipe) id: number,\n`;
    code += `    @Body() updateDto: Update${className}Dto,\n`;
    code += `  ): Promise<${className}ResponseDto> {\n`;
    code += `    return this.${this.toCamelCase(className)}Service.update(id, updateDto);\n`;
    code += `  }\n\n`;

    // DELETE
    code += `  @Delete(':id')\n`;
    code += `  @HttpCode(HttpStatus.NO_CONTENT)\n`;
    code += `  @ApiOperation({ summary: 'Delete ${routeName}' })\n`;
    code += `  @ApiResponse({ status: 204, description: '${className} deleted successfully' })\n`;
    code += `  @ApiResponse({ status: 404, description: '${className} not found' })\n`;
    code += `  async remove(@Param('id', ParseIntPipe) id: number): Promise<void> {\n`;
    code += `    await this.${this.toCamelCase(className)}Service.remove(id);\n`;
    code += `  }\n`;
    code += `}\n`;

    return code;
  }

  /**
   * Generate NestJS service
   */
  static generateNestJSService(entityName: string, usesPrisma: boolean = true): string {
    const className = this.toPascalCase(entityName);
    const orm = usesPrisma ? 'Prisma' : 'TypeORM';

    let code = `/**\n * NestJS Service for ${entityName}\n`;
    code += ` * Business logic layer\n */\n\n`;
    code += `import { Injectable, NotFoundException } from '@nestjs/common';\n`;

    if (usesPrisma) {
      code += `import { PrismaService } from '../prisma/prisma.service';\n`;
    } else {
      code += `import { InjectRepository } from '@nestjs/typeorm';\n`;
      code += `import { Repository } from 'typeorm';\n`;
      code += `import { ${className} } from './entities/${entityName.toLowerCase()}.entity';\n`;
    }

    code += `import { Create${className}Dto, Update${className}Dto } from './dto';\n\n`;

    code += `@Injectable()\n`;
    code += `export class ${className}Service {\n`;

    if (usesPrisma) {
      code += `  constructor(private prisma: PrismaService) {}\n\n`;
    } else {
      code += `  constructor(\n`;
      code += `    @InjectRepository(${className})\n`;
      code += `    private ${this.toCamelCase(className)}Repository: Repository<${className}>,\n`;
      code += `  ) {}\n\n`;
    }

    // findAll
    code += `  async findAll(options?: { skip?: number; take?: number }) {\n`;
    if (usesPrisma) {
      code += `    return this.prisma.${this.toCamelCase(className)}.findMany({\n`;
      code += `      skip: options?.skip || 0,\n`;
      code += `      take: options?.take || 100,\n`;
      code += `    });\n`;
    } else {
      code += `    return this.${this.toCamelCase(className)}Repository.find({\n`;
      code += `      skip: options?.skip || 0,\n`;
      code += `      take: options?.take || 100,\n`;
      code += `    });\n`;
    }
    code += `  }\n\n`;

    // findOne
    code += `  async findOne(id: number) {\n`;
    if (usesPrisma) {
      code += `    const item = await this.prisma.${this.toCamelCase(className)}.findUnique({\n`;
      code += `      where: { id },\n`;
      code += `    });\n`;
    } else {
      code += `    const item = await this.${this.toCamelCase(className)}Repository.findOne({\n`;
      code += `      where: { id },\n`;
      code += `    });\n`;
    }
    code += `    if (!item) {\n`;
    code += `      throw new NotFoundException(\`${className} with ID \${id} not found\`);\n`;
    code += `    }\n`;
    code += `    return item;\n`;
    code += `  }\n\n`;

    // create
    code += `  async create(createDto: Create${className}Dto) {\n`;
    if (usesPrisma) {
      code += `    return this.prisma.${this.toCamelCase(className)}.create({\n`;
      code += `      data: createDto,\n`;
      code += `    });\n`;
    } else {
      code += `    const item = this.${this.toCamelCase(className)}Repository.create(createDto);\n`;
      code += `    return this.${this.toCamelCase(className)}Repository.save(item);\n`;
    }
    code += `  }\n\n`;

    // update
    code += `  async update(id: number, updateDto: Update${className}Dto) {\n`;
    code += `    await this.findOne(id); // Check existence\n`;
    if (usesPrisma) {
      code += `    return this.prisma.${this.toCamelCase(className)}.update({\n`;
      code += `      where: { id },\n`;
      code += `      data: updateDto,\n`;
      code += `    });\n`;
    } else {
      code += `    await this.${this.toCamelCase(className)}Repository.update(id, updateDto);\n`;
      code += `    return this.findOne(id);\n`;
    }
    code += `  }\n\n`;

    // remove
    code += `  async remove(id: number): Promise<void> {\n`;
    code += `    await this.findOne(id); // Check existence\n`;
    if (usesPrisma) {
      code += `    await this.prisma.${this.toCamelCase(className)}.delete({\n`;
      code += `      where: { id },\n`;
      code += `    });\n`;
    } else {
      code += `    await this.${this.toCamelCase(className)}Repository.delete(id);\n`;
    }
    code += `  }\n`;
    code += `}\n`;

    return code;
  }

  /**
   * Generate package.json for NestJS project
   */
  static generatePackageJson(projectName: string, usePrisma: boolean = true): string {
    const dependencies: Record<string, string> = {
      "@nestjs/common": "^10.0.0",
      "@nestjs/core": "^10.0.0",
      "@nestjs/platform-express": "^10.0.0",
      "@nestjs/swagger": "^7.0.0",
      "reflect-metadata": "^0.1.13",
      "rxjs": "^7.8.0",
      "zod": "^3.22.0",
    };

    const devDependencies: Record<string, string> = {
      "@nestjs/cli": "^10.0.0",
      "@nestjs/schematics": "^10.0.0",
      "@nestjs/testing": "^10.0.0",
      "@types/express": "^4.17.17",
      "@types/node": "^20.3.1",
      "@typescript-eslint/eslint-plugin": "^6.0.0",
      "@typescript-eslint/parser": "^6.0.0",
      "eslint": "^8.42.0",
      "jest": "^29.5.0",
      "prettier": "^3.0.0",
      "ts-jest": "^29.1.0",
      "ts-node": "^10.9.1",
      "typescript": "^5.1.3",
    };

    if (usePrisma) {
      dependencies["@prisma/client"] = "^5.0.0";
      devDependencies["prisma"] = "^5.0.0";
    } else {
      dependencies["@nestjs/typeorm"] = "^10.0.0";
      dependencies["typeorm"] = "^0.3.17";
      dependencies["pg"] = "^8.11.0";
    }

    const packageJson = {
      name: projectName,
      version: "0.1.0",
      description: "Migrated from ABAP",
      scripts: {
        "build": "nest build",
        "start": "nest start",
        "start:dev": "nest start --watch",
        "start:prod": "node dist/main",
        "test": "jest",
        "test:watch": "jest --watch",
        "test:cov": "jest --coverage",
        "lint": "eslint \"{src,apps,libs,test}/**/*.ts\" --fix",
        "format": "prettier --write \"src/**/*.ts\" \"test/**/*.ts\"",
      },
      dependencies,
      devDependencies,
    };

    return JSON.stringify(packageJson, null, 2);
  }

  // Helper methods
  private static toPascalCase(str: string): string {
    return str
      .split(/[_-]/)
      .map(word => word.charAt(0).toUpperCase() + word.slice(1).toLowerCase())
      .join('');
  }

  private static toCamelCase(str: string): string {
    const pascal = this.toPascalCase(str);
    return pascal.charAt(0).toLowerCase() + pascal.slice(1);
  }

  private static mapToPrismaType(abapType: string): string {
    const mapping: Record<string, string> = {
      'i': 'Int',
      'int8': 'BigInt',
      'p': 'Decimal',
      'f': 'Float',
      'string': 'String',
      'c': 'String',
      'n': 'String',
      'd': 'DateTime',
      't': 'DateTime',
    };
    return mapping[abapType.toLowerCase()] || 'String';
  }

  private static mapToTypeScriptType(abapType: string): string {
    const mapping: Record<string, string> = {
      'i': 'number',
      'int8': 'bigint',
      'p': 'number',
      'f': 'number',
      'string': 'string',
      'c': 'string',
      'n': 'string',
      'd': 'Date',
      't': 'Date',
    };
    return mapping[abapType.toLowerCase()] || 'string';
  }

  private static mapToTypeORMColumnType(abapType: string): string {
    const mapping: Record<string, string> = {
      'i': '\'int\'',
      'int8': '\'bigint\'',
      'p': '{ type: \'decimal\', precision: 15, scale: 2 }',
      'f': '\'float\'',
      'string': '\'varchar\'',
      'c': '\'varchar\'',
      'n': '\'varchar\'',
      'd': '\'date\'',
      't': '\'time\'',
    };
    return mapping[abapType.toLowerCase()] || '\'varchar\'';
  }

  private static mapToZodType(abapType: string): string {
    const mapping: Record<string, string> = {
      'i': 'z.number().int()',
      'int8': 'z.bigint()',
      'p': 'z.number()',
      'f': 'z.number()',
      'string': 'z.string()',
      'c': 'z.string()',
      'n': 'z.string().regex(/^\\d+$/)',
      'd': 'z.date()',
      't': 'z.date()',
    };
    return mapping[abapType.toLowerCase()] || 'z.string()';
  }
}

// Example usage
if (require.main === module) {
  const fields = {
    'customer_id': 'n',
    'name': 'string',
    'amount': 'p',
    'created_date': 'd',
  };

  console.log('=== Prisma Schema ===');
  console.log(TypeScriptCodeGenerator.generatePrismaSchema('customer', fields));

  console.log('\n=== TypeORM Entity ===');
  console.log(TypeScriptCodeGenerator.generateTypeORMEntity('customer', fields));

  console.log('\n=== Zod Schema ===');
  console.log(TypeScriptCodeGenerator.generateZodSchema('customer', fields));

  console.log('\n=== NestJS Controller ===');
  console.log(TypeScriptCodeGenerator.generateNestJSController('customer', 'CustomerService'));
}
