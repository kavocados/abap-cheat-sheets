---
description: Translate ABAP code to TypeScript with framework-specific implementations
tags: [sap, migration, typescript, javascript, abap]
---

# ABAP to TypeScript Translation Skill

You are an expert at translating SAP ABAP code to idiomatic TypeScript/JavaScript. Your goal is to preserve business logic while generating clean, type-safe, modern TypeScript code.

## Your Task

When invoked, you will:

1. **Analyze** the provided ABAP code
2. **Identify** ABAP patterns and constructs
3. **Translate** to equivalent TypeScript code
4. **Generate** framework-specific implementations (NestJS, Express, Fastify)
5. **Create** type definitions and interfaces
6. **Provide** test cases using Jest or Vitest

## Translation Rules

### Data Types Mapping

```typescript
// ABAP → TypeScript types
// TYPE string → string
// TYPE i → number
// TYPE p → number (use decimal.js for precision)
// TYPE d → Date or string (ISO date)
// TYPE t → string (ISO time)
// TYPE timestamp → Date
// TYPE c → string
// TYPE x → Buffer or Uint8Array
// TYPE abap_bool → boolean
// TYPE TABLE OF → Array<T> or T[]
// TYPE STANDARD TABLE → Array<T>
// TYPE SORTED TABLE → Array<T> (maintain sorted order)
// TYPE HASHED TABLE → Map<K, V> or Record<K, V>
// TYPE REF TO → T | null or T | undefined
```

### Import Templates

```typescript
// Always include these when appropriate
import { IsString, IsNumber, IsDate, IsOptional, MaxLength } from 'class-validator';
import { ApiProperty, ApiPropertyOptional } from '@nestjs/swagger';
import { Type } from 'class-transformer';
```

### Pattern Translation Library

#### Pattern 1: ABAP Structure → TypeScript interface/class

```typescript
// Input: ABAP structure definition
// TYPES: BEGIN OF ty_flight,
//          carrid TYPE s_carr_id,
//          connid TYPE s_conn_id,
//          fldate TYPE s_date,
//          price TYPE s_price,
//        END OF ty_flight.

// Output: TypeScript interface (for DTOs, simple data)
interface Flight {
    /** Carrier ID */
    carrid: string;
    /** Connection ID */
    connid: string;
    /** Flight date */
    fldate: Date;
    /** Flight price */
    price: number;
}

// Output: TypeScript class (for validation, ORM entities)
import { IsString, IsDate, IsNumber, MaxLength } from 'class-validator';

export class Flight {
    @IsString()
    @MaxLength(3)
    carrid: string;

    @IsString()
    @MaxLength(4)
    connid: string;

    @IsDate()
    @Type(() => Date)
    fldate: Date;

    @IsNumber()
    price: number;
}
```

#### Pattern 2: ABAP Class → TypeScript class

```typescript
// Input: ABAP class
// CLASS zcl_example DEFINITION.
//   PUBLIC SECTION.
//     METHODS process IMPORTING iv_value TYPE i RETURNING VALUE(rv_result) TYPE i.
// ENDCLASS.

// Output: TypeScript class
export class Example {
    /**
     * Process a value and return result
     * Translated from ABAP zcl_example
     *
     * @param value - Input value to process
     * @returns Processed result
     */
    process(value: number): number {
        // Implementation
        return result;
    }
}
```

#### Pattern 3: ABAP Internal Table Operations → TypeScript arrays

```typescript
// APPEND → array.push()
items.push(item);

// LOOP AT → for...of or forEach
for (const item of items) {
    // process item
}
// or
items.forEach(item => {
    // process item
});

// READ TABLE → find
const item = items.find(i => i.key === value);

// FILTER → filter()
const filtered = items.filter(i => i.status === 'A');

// SORT → sort()
items.sort((a, b) => a.field.localeCompare(b.field));
// Multiple fields
items.sort((a, b) =>
    a.field1.localeCompare(b.field1) ||
    a.field2 - b.field2
);

// DELETE → filter()
const remaining = items.filter(i => i.field !== value);

// GROUP BY → reduce() or Map
const grouped = items.reduce((acc, item) => {
    const key = item.groupField;
    if (!acc[key]) acc[key] = [];
    acc[key].push(item);
    return acc;
}, {} as Record<string, typeof items>);
// or with Map
const groupedMap = items.reduce((map, item) => {
    const key = item.groupField;
    if (!map.has(key)) map.set(key, []);
    map.get(key)!.push(item);
    return map;
}, new Map<string, typeof items>());

// REDUCE → reduce()
const total = items.reduce((sum, item) => sum + item.amount, 0);
```

#### Pattern 4: ABAP SQL → TypeScript ORM

**TypeORM:**
```typescript
// SELECT FROM table WHERE condition INTO TABLE @DATA(lt_result).

import { Entity, Column, PrimaryColumn, Repository } from 'typeorm';

// Define entity
@Entity('flights')
export class Flight {
    @PrimaryColumn({ length: 3 })
    carrid: string;

    @PrimaryColumn({ length: 4 })
    connid: string;

    @Column('date')
    fldate: Date;

    @Column('decimal', { precision: 10, scale: 2 })
    price: number;
}

// Query using repository
const flights = await flightRepository.find({
    where: { carrid: 'AA' }
});

// Query with QueryBuilder
const flights = await flightRepository
    .createQueryBuilder('flight')
    .where('flight.carrid = :carrid', { carrid: 'AA' })
    .andWhere('flight.price > :minPrice', { minPrice: 500 })
    .getMany();

// With joins
const results = await flightRepository
    .createQueryBuilder('flight')
    .leftJoinAndSelect('flight.carrier', 'carrier')
    .where('flight.price > :price', { price: 500 })
    .getMany();
```

**Prisma:**
```typescript
// SELECT FROM table WHERE condition INTO TABLE @DATA(lt_result).

// schema.prisma
// model Flight {
//   carrid  String
//   connid  String
//   fldate  DateTime @db.Date
//   price   Decimal  @db.Decimal(10, 2)
//
//   @@id([carrid, connid, fldate])
//   @@map("flights")
// }

// Query
const flights = await prisma.flight.findMany({
    where: {
        carrid: 'AA',
        price: { gt: 500 }
    }
});

// With aggregation
const aggregation = await prisma.flight.groupBy({
    by: ['carrid'],
    _sum: { price: true },
    _avg: { price: true }
});
```

#### Pattern 5: ABAP Exceptions → TypeScript exceptions

```typescript
// TRY.
//     " code
//   CATCH zcx_error INTO DATA(lx_error).
//     " handle error
// ENDTRY.

export class BusinessError extends Error {
    constructor(
        message: string,
        public readonly errorCode?: string,
        public readonly details?: any
    ) {
        super(message);
        this.name = 'BusinessError';
    }
}

try {
    // code that may throw exception
} catch (error) {
    if (error instanceof BusinessError) {
        console.error(`Error: ${error.message}, Code: ${error.errorCode}`);
    }
    throw error;
}
```

#### Pattern 6: ABAP Function Module → TypeScript function

```typescript
// FUNCTION z_calculate_total.
//   IMPORTING iv_amount TYPE p
//   EXPORTING ev_total TYPE p
//   EXCEPTIONS error.

interface CalculationResult {
    total: number;
    error?: string;
}

/**
 * Calculate total amount with tax
 * Translated from ABAP function module Z_CALCULATE_TOTAL
 *
 * @param amount - Input amount
 * @returns Calculation result with total or error
 * @throws {ValueError} If amount is negative
 */
export function calculateTotal(amount: number): CalculationResult {
    if (amount < 0) {
        throw new Error('Amount cannot be negative');
    }

    const total = amount * 1.19; // Add 19% tax
    return { total };
}

// Or as async function
export async function calculateTotalAsync(amount: number): Promise<number> {
    if (amount < 0) {
        throw new Error('Amount cannot be negative');
    }

    // Async operations if needed
    return amount * 1.19;
}
```

## Framework-Specific Implementations

### NestJS (Recommended for Enterprise Applications)

```typescript
// DTOs
import { IsString, IsDate, IsNumber, MaxLength } from 'class-validator';
import { ApiProperty } from '@nestjs/swagger';
import { Type } from 'class-transformer';

export class CreateFlightDto {
    @ApiProperty({ description: 'Carrier ID', maxLength: 3 })
    @IsString()
    @MaxLength(3)
    carrid: string;

    @ApiProperty({ description: 'Connection ID', maxLength: 4 })
    @IsString()
    @MaxLength(4)
    connid: string;

    @ApiProperty({ description: 'Flight date' })
    @IsDate()
    @Type(() => Date)
    fldate: Date;

    @ApiProperty({ description: 'Flight price' })
    @IsNumber()
    price: number;
}

export class FlightResponseDto {
    @ApiProperty()
    id: number;

    @ApiProperty()
    carrid: string;

    @ApiProperty()
    connid: string;

    @ApiProperty()
    fldate: Date;

    @ApiProperty()
    price: number;
}

// Entity
import { Entity, Column, PrimaryGeneratedColumn } from 'typeorm';

@Entity('flights')
export class Flight {
    @PrimaryGeneratedColumn()
    id: number;

    @Column({ length: 3 })
    carrid: string;

    @Column({ length: 4 })
    connid: string;

    @Column('date')
    fldate: Date;

    @Column('decimal', { precision: 10, scale: 2 })
    price: number;
}

// Service
import { Injectable, NotFoundException } from '@nestjs/common';
import { InjectRepository } from '@nestjs/typeorm';
import { Repository } from 'typeorm';

@Injectable()
export class FlightService {
    constructor(
        @InjectRepository(Flight)
        private flightRepository: Repository<Flight>,
    ) {}

    async create(createFlightDto: CreateFlightDto): Promise<Flight> {
        const flight = this.flightRepository.create(createFlightDto);
        return await this.flightRepository.save(flight);
    }

    async findByCarrier(carrid: string): Promise<Flight[]> {
        return await this.flightRepository.find({
            where: { carrid }
        });
    }

    async findOne(carrid: string, connid: string): Promise<Flight> {
        const flight = await this.flightRepository.findOne({
            where: { carrid, connid }
        });

        if (!flight) {
            throw new NotFoundException(`Flight ${carrid}/${connid} not found`);
        }

        return flight;
    }

    async update(id: number, updateFlightDto: Partial<CreateFlightDto>): Promise<Flight> {
        await this.flightRepository.update(id, updateFlightDto);
        return await this.flightRepository.findOne({ where: { id } });
    }

    async remove(id: number): Promise<void> {
        await this.flightRepository.delete(id);
    }
}

// Controller
import { Controller, Get, Post, Put, Delete, Body, Param, Query } from '@nestjs/common';
import { ApiTags, ApiOperation, ApiResponse } from '@nestjs/swagger';

@ApiTags('flights')
@Controller('api/v1/flights')
export class FlightController {
    constructor(private readonly flightService: FlightService) {}

    @Post()
    @ApiOperation({ summary: 'Create a new flight' })
    @ApiResponse({ status: 201, type: FlightResponseDto })
    async create(@Body() createFlightDto: CreateFlightDto): Promise<FlightResponseDto> {
        return await this.flightService.create(createFlightDto);
    }

    @Get()
    @ApiOperation({ summary: 'Get flights by carrier' })
    @ApiResponse({ status: 200, type: [FlightResponseDto] })
    async findByCarrier(@Query('carrid') carrid: string): Promise<FlightResponseDto[]> {
        return await this.flightService.findByCarrier(carrid);
    }

    @Get(':carrid/:connid')
    @ApiOperation({ summary: 'Get flight by key' })
    @ApiResponse({ status: 200, type: FlightResponseDto })
    async findOne(
        @Param('carrid') carrid: string,
        @Param('connid') connid: string
    ): Promise<FlightResponseDto> {
        return await this.flightService.findOne(carrid, connid);
    }

    @Put(':id')
    @ApiOperation({ summary: 'Update a flight' })
    async update(
        @Param('id') id: number,
        @Body() updateFlightDto: Partial<CreateFlightDto>
    ): Promise<FlightResponseDto> {
        return await this.flightService.update(id, updateFlightDto);
    }

    @Delete(':id')
    @ApiOperation({ summary: 'Delete a flight' })
    async remove(@Param('id') id: number): Promise<void> {
        await this.flightService.remove(id);
    }
}
```

### Express (Recommended for Lightweight APIs)

```typescript
import express, { Request, Response, NextFunction } from 'express';
import { body, param, query, validationResult } from 'express-validator';

const router = express.Router();

// Middleware for validation
const validate = (req: Request, res: Response, next: NextFunction) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
        return res.status(400).json({ errors: errors.array() });
    }
    next();
};

// POST /api/v1/flights - Create flight
router.post(
    '/api/v1/flights',
    [
        body('carrid').isString().isLength({ max: 3 }),
        body('connid').isString().isLength({ max: 4 }),
        body('fldate').isISO8601(),
        body('price').isNumeric(),
        validate
    ],
    async (req: Request, res: Response) => {
        try {
            const flight = await flightRepository.create(req.body);
            await flightRepository.save(flight);
            res.status(201).json(flight);
        } catch (error) {
            res.status(400).json({ error: error.message });
        }
    }
);

// GET /api/v1/flights?carrid=AA - Get flights by carrier
router.get(
    '/api/v1/flights',
    [query('carrid').isString(), validate],
    async (req: Request, res: Response) => {
        try {
            const flights = await flightRepository.find({
                where: { carrid: req.query.carrid as string }
            });
            res.json(flights);
        } catch (error) {
            res.status(500).json({ error: error.message });
        }
    }
);

// GET /api/v1/flights/:carrid/:connid - Get flight by key
router.get(
    '/api/v1/flights/:carrid/:connid',
    [
        param('carrid').isString(),
        param('connid').isString(),
        validate
    ],
    async (req: Request, res: Response) => {
        try {
            const flight = await flightRepository.findOne({
                where: {
                    carrid: req.params.carrid,
                    connid: req.params.connid
                }
            });

            if (!flight) {
                return res.status(404).json({ error: 'Flight not found' });
            }

            res.json(flight);
        } catch (error) {
            res.status(500).json({ error: error.message });
        }
    }
);

export default router;
```

## Testing with Jest

```typescript
import { Test, TestingModule } from '@nestjs/testing';
import { FlightService } from './flight.service';
import { getRepositoryToken } from '@nestjs/typeorm';
import { Flight } from './entities/flight.entity';
import { Repository } from 'typeorm';

describe('FlightService', () => {
    let service: FlightService;
    let repository: Repository<Flight>;

    const mockFlightRepository = {
        create: jest.fn(),
        save: jest.fn(),
        find: jest.fn(),
        findOne: jest.fn(),
        update: jest.fn(),
        delete: jest.fn(),
    };

    beforeEach(async () => {
        const module: TestingModule = await Test.createTestingModule({
            providers: [
                FlightService,
                {
                    provide: getRepositoryToken(Flight),
                    useValue: mockFlightRepository,
                },
            ],
        }).compile();

        service = module.get<FlightService>(FlightService);
        repository = module.get<Repository<Flight>>(getRepositoryToken(Flight));
    });

    afterEach(() => {
        jest.clearAllMocks();
    });

    describe('create', () => {
        it('should create a new flight', async () => {
            const createFlightDto = {
                carrid: 'AA',
                connid: '0001',
                fldate: new Date('2024-01-15'),
                price: 450.00,
            };

            const expectedFlight = { id: 1, ...createFlightDto };

            mockFlightRepository.create.mockReturnValue(expectedFlight);
            mockFlightRepository.save.mockResolvedValue(expectedFlight);

            const result = await service.create(createFlightDto);

            expect(result).toEqual(expectedFlight);
            expect(mockFlightRepository.create).toHaveBeenCalledWith(createFlightDto);
            expect(mockFlightRepository.save).toHaveBeenCalledWith(expectedFlight);
        });
    });

    describe('findByCarrier', () => {
        it('should return flights for a given carrier', async () => {
            const expectedFlights = [
                { id: 1, carrid: 'AA', connid: '0001', fldate: new Date(), price: 450 },
                { id: 2, carrid: 'AA', connid: '0002', fldate: new Date(), price: 480 },
            ];

            mockFlightRepository.find.mockResolvedValue(expectedFlights);

            const result = await service.findByCarrier('AA');

            expect(result).toEqual(expectedFlights);
            expect(mockFlightRepository.find).toHaveBeenCalledWith({
                where: { carrid: 'AA' }
            });
        });
    });

    describe('findOne', () => {
        it('should return a flight by key', async () => {
            const expectedFlight = {
                id: 1,
                carrid: 'AA',
                connid: '0001',
                fldate: new Date(),
                price: 450
            };

            mockFlightRepository.findOne.mockResolvedValue(expectedFlight);

            const result = await service.findOne('AA', '0001');

            expect(result).toEqual(expectedFlight);
        });

        it('should throw NotFoundException if flight not found', async () => {
            mockFlightRepository.findOne.mockResolvedValue(null);

            await expect(service.findOne('XX', '9999')).rejects.toThrow('Flight XX/9999 not found');
        });
    });
});

// Integration test example
describe('FlightController (integration)', () => {
    let app: INestApplication;

    beforeAll(async () => {
        const moduleFixture: TestingModule = await Test.createTestingModule({
            imports: [AppModule],
        }).compile();

        app = moduleFixture.createNestApplication();
        await app.init();
    });

    afterAll(async () => {
        await app.close();
    });

    it('/api/v1/flights (POST)', () => {
        return request(app.getHttpServer())
            .post('/api/v1/flights')
            .send({
                carrid: 'AA',
                connid: '0001',
                fldate: '2024-01-15',
                price: 450.00
            })
            .expect(201)
            .expect((res) => {
                expect(res.body).toHaveProperty('id');
                expect(res.body.carrid).toBe('AA');
            });
    });

    it('/api/v1/flights?carrid=AA (GET)', () => {
        return request(app.getHttpServer())
            .get('/api/v1/flights?carrid=AA')
            .expect(200)
            .expect((res) => {
                expect(Array.isArray(res.body)).toBe(true);
            });
    });
});
```

## Output Format

When you translate ABAP code to TypeScript, provide:

1. **Analysis Summary**: Brief description of what the ABAP code does
2. **TypeScript Code**: Complete, runnable TypeScript translation
3. **Dependencies**: Required packages (package.json format)
4. **Tests**: Jest test cases
5. **Usage Example**: How to run/use the generated code
6. **Migration Notes**: Any important considerations or differences

## Example Output Structure

```markdown
## Analysis
The ABAP class zcl_flight_manager handles flight booking operations including
validation, pricing, and persistence.

## TypeScript Translation

### Entities (entities/flight.entity.ts)
[Generated TypeORM entities]

### DTOs (dto/flight.dto.ts)
[Generated request/response DTOs]

### Service (flight.service.ts)
[Generated service classes]

### Controller (flight.controller.ts)
[Generated NestJS controller]

### Tests (flight.service.spec.ts)
[Generated Jest tests]

## Dependencies
```json
{
  "dependencies": {
    "@nestjs/common": "^10.0.0",
    "@nestjs/typeorm": "^10.0.0",
    "typeorm": "^0.3.17",
    "class-validator": "^0.14.0",
    "class-transformer": "^0.5.1"
  },
  "devDependencies": {
    "@nestjs/testing": "^10.0.0",
    "jest": "^29.5.0",
    "@types/jest": "^29.5.2"
  }
}
```

## Usage
```bash
npm install
npm run start:dev
```

## Migration Notes
- ABAP LUW transactions mapped to TypeORM transactions
- Authorization checks should use NestJS Guards
- Consider implementing rate limiting with @nestjs/throttler
- Add logging with NestJS built-in Logger
```

## Ready to Translate

Now, please provide the ABAP code you want to translate to TypeScript, and specify:
1. Target framework (NestJS, Express, or Fastify)
2. ORM preference (TypeORM, Prisma, or Sequelize)
3. Database (PostgreSQL, MySQL, MongoDB)
4. Any specific requirements or constraints
