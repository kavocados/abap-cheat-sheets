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

#### Pattern 7: ABAP Number Ranges → TypeScript nanoid/ulid

```typescript
// Input: ABAP number range usage
// CALL FUNCTION 'NUMBER_GET_NEXT'
//   EXPORTING
//     nr_range_nr = '01'
//     object      = 'ZINVOICE'
//   IMPORTING
//     number      = lv_invoice_nr.

// Output Option 1: nanoid (short, URL-safe, globally unique)
import { nanoid } from 'nanoid';

interface Invoice {
    id: string;
    invoiceNumber: string;
    createdAt: Date;
}

function createInvoice(): Invoice {
    const id = nanoid(); // Short unique ID
    // Format: INV-YYYYMMDD-XXXXXXXX
    const date = new Date().toISOString().slice(0, 10).replace(/-/g, '');
    const invoiceNumber = `INV-${date}-${nanoid(8).toUpperCase()}`;

    return {
        id,
        invoiceNumber,
        createdAt: new Date()
    };
}

// Output Option 2: ulid (sortable, timestamp-based, globally unique)
import { ulid } from 'ulid';

function createInvoiceWithUlid(): string {
    return ulid(); // Sortable by creation time
}

// Output Option 3: Database auto-increment (TypeORM)
import { Entity, Column, PrimaryGeneratedColumn } from 'typeorm';

@Entity('invoices')
export class Invoice {
    @PrimaryGeneratedColumn()
    id: number;

    @Column({ unique: true })
    invoiceNumber: string;

    @Column({ type: 'timestamp', default: () => 'CURRENT_TIMESTAMP' })
    createdAt: Date;
}

// Output Option 4: Redis-based distributed ID (NestJS)
import { Injectable } from '@nestjs/common';
import { InjectRedis } from '@liaoliaots/nestjs-redis';
import Redis from 'ioredis';

@Injectable()
export class IdGeneratorService {
    constructor(@InjectRedis() private readonly redis: Redis) {}

    async getNextId(prefix: string = 'INV', rangeName: string = 'default'): Promise<string> {
        // Atomic increment in Redis
        const counter = await this.redis.incr(`nr_range:${rangeName}`);

        // Format: PREFIX-YYYYMMDD-000001
        const date = new Date().toISOString().slice(0, 10).replace(/-/g, '');
        return `${prefix}-${date}-${counter.toString().padStart(6, '0')}`;
    }
}
```

#### Pattern 8: ABAP Authorization → TypeScript Guards/CASL

```typescript
// Input: ABAP authorization check
// AUTHORITY-CHECK OBJECT 'Z_FLIGHT'
//   ID 'CARRID' FIELD lv_carrid
//   ID 'ACTVT'  FIELD '03'.  "Display
// IF sy-subrc <> 0.
//   RAISE EXCEPTION TYPE zcx_no_authorization.
// ENDIF.

// Output Option 1: NestJS Guards (simple decorator-based)
import { Injectable, CanActivate, ExecutionContext, ForbiddenException } from '@nestjs/common';
import { Reflector } from '@nestjs/core';
import { SetMetadata } from '@nestjs/common';

// Permission decorator
export const RequirePermission = (permission: string) => SetMetadata('permission', permission);

@Injectable()
export class PermissionGuard implements CanActivate {
    constructor(private reflector: Reflector) {}

    canActivate(context: ExecutionContext): boolean {
        const requiredPermission = this.reflector.get<string>('permission', context.getHandler());

        if (!requiredPermission) {
            return true;
        }

        const request = context.switchToHttp().getRequest();
        const user = request.user;

        if (!user || !user.permissions.includes(requiredPermission)) {
            throw new ForbiddenException(`Permission denied: ${requiredPermission} required`);
        }

        return true;
    }
}

// Usage
import { Controller, Get, UseGuards } from '@nestjs/common';

@Controller('flights')
@UseGuards(PermissionGuard)
export class FlightController {
    @Get(':carrid')
    @RequirePermission('flight:read')
    async getFlight(@Param('carrid') carrid: string) {
        return await this.flightService.getByCarrier(carrid);
    }

    @Post()
    @RequirePermission('flight:write')
    async createFlight(@Body() createDto: CreateFlightDto) {
        return await this.flightService.create(createDto);
    }
}

// Output Option 2: CASL for complex authorization (ABAP-like rules)
import { Injectable } from '@nestjs/common';
import { Ability, AbilityBuilder, AbilityClass, ExtractSubjectType, InferSubjects } from '@casl/ability';

type Actions = 'read' | 'create' | 'update' | 'delete';
type Subjects = InferSubjects<typeof Flight | typeof Booking> | 'all';

export type AppAbility = Ability<[Actions, Subjects]>;

@Injectable()
export class CaslAbilityFactory {
    createForUser(user: User) {
        const { can, cannot, build } = new AbilityBuilder<AppAbility>(Ability as AbilityClass<AppAbility>);

        if (user.role === 'admin') {
            can('manage', 'all'); // Admin can do everything
        } else if (user.role === 'user') {
            can('read', Flight);
            can('create', Booking, { userId: user.id }); // Can only create own bookings
            can('update', Booking, { userId: user.id });
        }

        // Field-level authorization (like ABAP ID fields)
        can('read', Flight, ['carrid', 'connid', 'price'], { carrid: { $in: user.allowedCarriers } });

        return build({
            detectSubjectType: (item) => item.constructor as ExtractSubjectType<Subjects>,
        });
    }
}

// Usage in service
@Injectable()
export class FlightService {
    constructor(private caslAbilityFactory: CaslAbilityFactory) {}

    async findOne(id: number, user: User) {
        const ability = this.caslAbilityFactory.createForUser(user);
        const flight = await this.flightRepository.findOne({ where: { id } });

        if (!ability.can('read', flight)) {
            throw new ForbiddenException('Cannot read this flight');
        }

        return flight;
    }
}
```

#### Pattern 9: ABAP Background Jobs → TypeScript Bull Queues

```typescript
// Input: ABAP background job
// CALL FUNCTION 'JOB_OPEN'
//   EXPORTING jobname = 'ZINVOICE_PROCESS'
//   IMPORTING jobcount = lv_jobcount.
// SUBMIT zinvoice_processor VIA JOB 'ZINVOICE_PROCESS' NUMBER lv_jobcount.
// CALL FUNCTION 'JOB_CLOSE' EXPORTING jobcount = lv_jobcount.

// Output: Bull queue (NestJS)
import { Injectable } from '@nestjs/common';
import { Queue } from 'bull';
import { InjectQueue, Process, Processor } from '@nestjs/bull';
import { Job } from 'bull';

// Define job processor
@Processor('invoices')
export class InvoiceProcessor {
    @Process('process-pending')
    async handleProcessInvoices(job: Job): Promise<{processed: number, errors: number}> {
        const logger = job.log.bind(job);
        logger('Starting invoice processing job');

        try {
            const pending = await this.invoiceRepository.find({ where: { status: 'pending' } });

            let processed = 0;
            let errors = 0;

            for (const invoice of pending) {
                try {
                    await this.processInvoice(invoice);
                    processed++;
                } catch (error) {
                    logger(`Error processing invoice ${invoice.id}: ${error.message}`);
                    errors++;
                }
            }

            logger(`Job complete: ${processed} processed, ${errors} errors`);
            return { processed, errors };
        } catch (error) {
            logger(`Job failed: ${error.message}`);
            throw error;
        }
    }

    private async processInvoice(invoice: Invoice): Promise<void> {
        // Business logic here
    }
}

// Service to submit jobs
@Injectable()
export class InvoiceService {
    constructor(@InjectQueue('invoices') private invoiceQueue: Queue) {}

    // Submit single job (immediate)
    async submitProcessingJob(): Promise<void> {
        await this.invoiceQueue.add('process-pending', {}, {
            attempts: 3,
            backoff: {
                type: 'exponential',
                delay: 2000
            }
        });
    }

    // Schedule job (like SM36)
    async scheduleProcessingJob(delayMs: number): Promise<void> {
        await this.invoiceQueue.add('process-pending', {}, {
            delay: delayMs
        });
    }

    // Periodic job (cron)
    async setupPeriodicJob(): Promise<void> {
        await this.invoiceQueue.add('process-pending', {}, {
            repeat: {
                cron: '0 * * * *' // Every hour
            }
        });
    }
}

// Module configuration
import { BullModule } from '@nestjs/bull';

@Module({
    imports: [
        BullModule.forRoot({
            redis: {
                host: 'localhost',
                port: 6379,
            },
        }),
        BullModule.registerQueue({
            name: 'invoices',
        }),
    ],
    providers: [InvoiceService, InvoiceProcessor],
})
export class InvoiceModule {}
```

#### Pattern 10: ABAP Lock Objects → TypeScript Redlock

```typescript
// Input: ABAP enqueue/dequeue
// CALL FUNCTION 'ENQUEUE_EZFLIGHT'
//   EXPORTING mode_zdemo_abap_fli = 'E' carrid = lv_carrid connid = lv_connid
//   EXCEPTIONS foreign_lock = 1.
// " ... critical section ...
// CALL FUNCTION 'DEQUEUE_EZFLIGHT' EXPORTING carrid = lv_carrid connid = lv_connid.

// Output: Redlock distributed locking (NestJS)
import { Injectable } from '@nestjs/common';
import Redis from 'ioredis';
import Redlock from 'redlock';

@Injectable()
export class LockService {
    private redlock: Redlock;

    constructor() {
        const redisClient = new Redis();
        this.redlock = new Redlock([redisClient], {
            retryCount: 10,
            retryDelay: 200,
        });
    }

    async withLock<T>(
        lockKey: string,
        callback: () => Promise<T>,
        duration: number = 10000
    ): Promise<T> {
        const lock = await this.redlock.acquire([`lock:${lockKey}`], duration);

        try {
            return await callback();
        } finally {
            await lock.release();
        }
    }
}

// Usage
@Injectable()
export class FlightService {
    constructor(private lockService: LockService) {}

    async updateFlight(carrid: string, connid: string, updates: Partial<Flight>): Promise<Flight> {
        const lockKey = `flight:${carrid}:${connid}`;

        return await this.lockService.withLock(lockKey, async () => {
            // Critical section - update flight data
            const flight = await this.flightRepository.findOne({ where: { carrid, connid } });
            Object.assign(flight, updates);
            return await this.flightRepository.save(flight);
        });
    }
}

// Alternative: TypeORM pessimistic locking (single server)
import { DataSource, QueryRunner } from 'typeorm';

@Injectable()
export class FlightService {
    async updateFlightWithDbLock(carrid: string, connid: string): Promise<Flight> {
        const queryRunner: QueryRunner = this.dataSource.createQueryRunner();
        await queryRunner.connect();
        await queryRunner.startTransaction();

        try {
            // SELECT FOR UPDATE (database-level lock)
            const flight = await queryRunner.manager.findOne(Flight, {
                where: { carrid, connid },
                lock: { mode: 'pessimistic_write' }
            });

            // Critical section
            flight.seatsOccupied += 1;
            await queryRunner.manager.save(flight);

            await queryRunner.commitTransaction();
            return flight;
        } catch (error) {
            await queryRunner.rollbackTransaction();
            throw error;
        } finally {
            await queryRunner.release();
        }
    }
}
```

#### Pattern 11: ABAP Message Classes → TypeScript i18next

```typescript
// Input: ABAP message class
// MESSAGE e001(zinvoice) WITH lv_invoice_nr INTO lv_msg.
// " Message: Invoice &1 not found

// Output: i18next for internationalization
import i18next from 'i18next';
import { HttpException, HttpStatus } from '@nestjs/common';

// Initialize i18next
i18next.init({
    lng: 'en',
    resources: {
        en: {
            invoice: {
                'E001': 'Invoice {{0}} not found',
                'E002': 'Invoice {{0}} already processed',
                'W001': 'Invoice {{0}} has no line items',
                'I001': 'Invoice {{0}} created successfully'
            }
        },
        de: {
            invoice: {
                'E001': 'Rechnung {{0}} nicht gefunden',
                'E002': 'Rechnung {{0}} bereits verarbeitet',
                'W001': 'Rechnung {{0}} hat keine Positionen',
                'I001': 'Rechnung {{0}} erfolgreich erstellt'
            }
        }
    }
});

// Message class handler
enum MessageType {
    ERROR = 'E',
    WARNING = 'W',
    INFO = 'I',
    SUCCESS = 'S'
}

class MessageClass {
    constructor(private messageClass: string, private locale: string = 'en') {}

    getMessage(msgType: MessageType, msgNumber: string, ...params: any[]): string {
        const msgKey = `${msgType}${msgNumber}`;
        const template = i18next.t(`${this.messageClass}.${msgKey}`, { lng: this.locale });

        // Replace placeholders
        return params.reduce((msg, param, index) => {
            return msg.replace(`{{${index}}}`, param);
        }, template);
    }
}

// Business exception with message class
export class BusinessException extends HttpException {
    constructor(
        messageClass: string,
        msgType: MessageType,
        msgNumber: string,
        params: any[] = [],
        locale: string = 'en'
    ) {
        const messages = new MessageClass(messageClass, locale);
        const message = messages.getMessage(msgType, msgNumber, ...params);

        const statusCode = {
            [MessageType.ERROR]: HttpStatus.BAD_REQUEST,
            [MessageType.WARNING]: HttpStatus.BAD_REQUEST,
            [MessageType.INFO]: HttpStatus.OK,
        }[msgType] || HttpStatus.INTERNAL_SERVER_ERROR;

        super(message, statusCode);
    }
}

// Usage in service
if (!invoice) {
    throw new BusinessException('invoice', MessageType.ERROR, '001', [invoiceNr]);
}
```

#### Pattern 12: ABAP Selection Screens → TypeScript Query DTOs

```typescript
// Input: ABAP selection screen
// PARAMETERS: p_carrid TYPE s_carr_id.
// SELECT-OPTIONS: s_connid FOR ls_flight-connid.
// SELECT-OPTIONS: s_date FOR ls_flight-fldate.

// Output: Query DTO with class-validator
import { IsString, IsOptional, IsInt, Min, Max, IsDateString, ValidateNested } from 'class-validator';
import { Type } from 'class-transformer';
import { ApiProperty, ApiPropertyOptional } from '@nestjs/swagger';

class DateRange {
    @ApiPropertyOptional({ description: 'Start date (inclusive)' })
    @IsOptional()
    @IsDateString()
    fromDate?: string;

    @ApiPropertyOptional({ description: 'End date (inclusive)' })
    @IsOptional()
    @IsDateString()
    toDate?: string;
}

export class FlightQueryDto {
    // PARAMETERS equivalent
    @ApiPropertyOptional({ description: 'Carrier ID', maxLength: 3 })
    @IsOptional()
    @IsString()
    carrid?: string;

    // SELECT-OPTIONS equivalent (range)
    @ApiPropertyOptional({ description: 'Connection ID from' })
    @IsOptional()
    @IsString()
    connidFrom?: string;

    @ApiPropertyOptional({ description: 'Connection ID to' })
    @IsOptional()
    @IsString()
    connidTo?: string;

    // SELECT-OPTIONS with multiple values
    @ApiPropertyOptional({ description: 'List of connection IDs', type: [String] })
    @IsOptional()
    @IsString({ each: true })
    connidIn?: string[];

    // Date range
    @ApiPropertyOptional({ type: DateRange })
    @IsOptional()
    @ValidateNested()
    @Type(() => DateRange)
    flightDate?: DateRange;

    // PARAMETERS with default
    @ApiProperty({ description: 'Maximum rows', default: 100 })
    @IsInt()
    @Min(1)
    @Max(1000)
    maxRows: number = 100;
}

// Controller usage
@Controller('flights')
export class FlightController {
    @Get('search')
    async searchFlights(@Query() query: FlightQueryDto) {
        const queryBuilder = this.flightRepository.createQueryBuilder('flight');

        // Apply filters
        if (query.carrid) {
            queryBuilder.andWhere('flight.carrid = :carrid', { carrid: query.carrid });
        }

        if (query.connidFrom && query.connidTo) {
            queryBuilder.andWhere('flight.connid BETWEEN :from AND :to', {
                from: query.connidFrom,
                to: query.connidTo
            });
        }

        if (query.connidIn && query.connidIn.length > 0) {
            queryBuilder.andWhere('flight.connid IN (:...connids)', { connids: query.connidIn });
        }

        if (query.flightDate?.fromDate) {
            queryBuilder.andWhere('flight.fldate >= :fromDate', { fromDate: query.flightDate.fromDate });
        }

        if (query.flightDate?.toDate) {
            queryBuilder.andWhere('flight.fldate <= :toDate', { toDate: query.flightDate.toDate });
        }

        queryBuilder.limit(query.maxRows);

        const results = await queryBuilder.getMany();
        return { count: results.length, maxRows: query.maxRows, data: results };
    }
}
```

#### Pattern 13: ABAP ALV Grids → TypeScript Pagination

```typescript
// Input: ABAP ALV grid display
// CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
//   EXPORTING it_fieldcat = lt_fieldcat
//   TABLES t_outtab = lt_flights.

// Output: NestJS with pagination
import { ApiProperty } from '@nestjs/swagger';

export class PageParams {
    @ApiProperty({ default: 1, minimum: 1 })
    @IsInt()
    @Min(1)
    page: number = 1;

    @ApiProperty({ default: 50, minimum: 1, maximum: 1000 })
    @IsInt()
    @Min(1)
    @Max(1000)
    pageSize: number = 50;

    @ApiPropertyOptional()
    @IsOptional()
    @IsString()
    sortBy?: string;

    @ApiProperty({ enum: ['asc', 'desc'], default: 'asc' })
    sortOrder: 'asc' | 'desc' = 'asc';
}

export class PagedResponse<T> {
    @ApiProperty()
    total: number;

    @ApiProperty()
    page: number;

    @ApiProperty()
    pageSize: number;

    @ApiProperty()
    totalPages: number;

    @ApiProperty({ isArray: true })
    data: T[];
}

@Controller('flights')
export class FlightController {
    @Get('grid')
    async getFlightsGrid(
        @Query() pageParams: PageParams,
        @Query() filters: FlightFilterDto
    ): Promise<PagedResponse<Flight>> {
        const queryBuilder = this.flightRepository.createQueryBuilder('flight');

        // Apply filters
        if (filters.carrid) queryBuilder.andWhere('flight.carrid = :carrid', { carrid: filters.carrid });
        if (filters.minPrice) queryBuilder.andWhere('flight.price >= :minPrice', { minPrice: filters.minPrice });

        // Get total count
        const total = await queryBuilder.getCount();

        // Apply sorting
        if (pageParams.sortBy) {
            queryBuilder.orderBy(`flight.${pageParams.sortBy}`, pageParams.sortOrder.toUpperCase() as 'ASC' | 'DESC');
        }

        // Apply pagination
        const offset = (pageParams.page - 1) * pageParams.pageSize;
        queryBuilder.skip(offset).take(pageParams.pageSize);

        const data = await queryBuilder.getMany();
        const totalPages = Math.ceil(total / pageParams.pageSize);

        return { total, page: pageParams.page, pageSize: pageParams.pageSize, totalPages, data };
    }

    // CSV export (ALV export equivalent)
    @Get('export/csv')
    async exportCsv(@Res() res: Response) {
        const flights = await this.flightRepository.find();
        const csv = this.convertToCSV(flights);

        res.setHeader('Content-Type', 'text/csv');
        res.setHeader('Content-Disposition', 'attachment; filename=flights.csv');
        res.send(csv);
    }
}
```

#### Pattern 14: ABAP Synchronous Code → TypeScript Async/Await

```typescript
// Input: Synchronous ABAP code
// SELECT * FROM zdemo_abap_fli INTO TABLE @DATA(lt_flights).
// LOOP AT lt_flights INTO DATA(ls_flight).
//   CALL FUNCTION 'Z_GET_FLIGHT_DETAILS'...
// ENDLOOP.

// Output: Async TypeScript with parallel processing
import { Injectable, HttpService } from '@nestjs/common';

@Injectable()
export class FlightService {
    constructor(private httpService: HttpService) {}

    // Async database query
    async getFlights(carrid?: string): Promise<Flight[]> {
        const queryBuilder = this.flightRepository.createQueryBuilder('flight');
        if (carrid) {
            queryBuilder.where('flight.carrid = :carrid', { carrid });
        }
        return await queryBuilder.getMany();
    }

    // Async external service call
    async getFlightDetails(carrid: string, connid: string): Promise<any> {
        const response = await this.httpService.axiosRef.get(
            `https://api.example.com/flights/${carrid}/${connid}`
        );
        return response.data;
    }

    // Parallel processing (much faster than LOOP)
    async enrichFlightsParallel(flights: Flight[]): Promise<any[]> {
        // Create promises for all flights
        const promises = flights.map(flight =>
            this.getFlightDetails(flight.carrid, flight.connid)
        );

        // Execute all in parallel
        const detailsList = await Promise.all(promises);

        // Combine results
        return flights.map((flight, index) => ({
            ...flight,
            details: detailsList[index]
        }));
    }

    // Controller usage
    @Get('enriched')
    async getEnrichedFlights(@Query('carrid') carrid?: string) {
        const flights = await this.getFlights(carrid);
        const enriched = await this.enrichFlightsParallel(flights);
        return { count: enriched.length, data: enriched };
    }
}
```

#### Pattern 15: ABAP Events → TypeScript Event Emitter/CQRS

```typescript
// Input: ABAP event handling
// CLASS lcl_handler DEFINITION.
//   PUBLIC SECTION.
//     METHODS on_data_changed FOR EVENT data_changed...
// ENDCLASS.

// Output: NestJS Event Emitter
import { Injectable } from '@nestjs/common';
import { EventEmitter2, OnEvent } from '@nestjs/event-emitter';

// Event classes
export class FlightCreatedEvent {
    constructor(
        public readonly flightId: number,
        public readonly carrid: string,
        public readonly connid: string
    ) {}
}

export class BookingCreatedEvent {
    constructor(public readonly bookingId: number, public readonly flightId: number) {}
}

// Event publisher (business logic)
@Injectable()
export class FlightService {
    constructor(private eventEmitter: EventEmitter2) {}

    async createFlight(createDto: CreateFlightDto): Promise<Flight> {
        const flight = this.flightRepository.create(createDto);
        await this.flightRepository.save(flight);

        // Publish event
        this.eventEmitter.emit(
            'flight.created',
            new FlightCreatedEvent(flight.id, flight.carrid, flight.connid)
        );

        return flight;
    }
}

// Event listeners (subscribers)
@Injectable()
export class FlightEventListeners {
    @OnEvent('flight.created')
    async handleFlightCreated(event: FlightCreatedEvent) {
        console.log(`Flight created: ${event.carrid}/${event.connid}`);
        // Send notification, update cache, etc.
    }

    @OnEvent('booking.created')
    async handleBookingCreated(event: BookingCreatedEvent) {
        // Update inventory, send confirmation email
        await this.emailService.sendConfirmation(event.bookingId);
    }
}

// Module setup
@Module({
    imports: [
        EventEmitterModule.forRoot()
    ],
    providers: [FlightService, FlightEventListeners]
})
export class FlightModule {}

// For distributed systems: Use NestJS Microservices with RabbitMQ/Kafka
import { Controller } from '@nestjs/common';
import { EventPattern, MessagePattern } from '@nestjs/microservices';

@Controller()
export class EventController {
    @EventPattern('flight_created')
    async handleFlightCreated(data: FlightCreatedEvent) {
        // Handle event from message queue
        console.log(`Received flight created event: ${data.flightId}`);
    }
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

## Deployment and Infrastructure

### Docker Configuration

**Dockerfile for NestJS:**
```dockerfile
# Multi-stage build for production
FROM node:18-alpine AS builder

WORKDIR /app

# Copy package files
COPY package*.json ./
COPY tsconfig*.json ./

# Install dependencies
RUN npm ci

# Copy source code
COPY src ./src

# Build application
RUN npm run build

# Production stage
FROM node:18-alpine AS production

WORKDIR /app

# Install production dependencies only
COPY package*.json ./
RUN npm ci --only=production && npm cache clean --force

# Copy built application from builder
COPY --from=builder /app/dist ./dist

# Create non-root user
RUN addgroup -g 1000 appuser && \
    adduser -D -u 1000 -G appuser appuser && \
    chown -R appuser:appuser /app

USER appuser

EXPOSE 3000

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD node -e "require('http').get('http://localhost:3000/health', (r) => {process.exit(r.statusCode === 200 ? 0 : 1)})"

CMD ["node", "dist/main"]
```

**docker-compose.yml:**
```yaml
version: '3.8'

services:
  api:
    build:
      context: .
      target: production
    ports:
      - "3000:3000"
    environment:
      - NODE_ENV=production
      - DATABASE_URL=postgresql://user:password@db:5432/appdb
      - REDIS_URL=redis://redis:6379
      - JWT_SECRET=${JWT_SECRET}
    depends_on:
      db:
        condition: service_healthy
      redis:
        condition: service_healthy
    volumes:
      - ./logs:/app/logs
    restart: unless-stopped
    networks:
      - app-network

  db:
    image: postgres:15-alpine
    environment:
      - POSTGRES_USER=user
      - POSTGRES_PASSWORD=password
      - POSTGRES_DB=appdb
    volumes:
      - postgres_data:/var/lib/postgresql/data
      - ./init-scripts:/docker-entrypoint-initdb.d
    ports:
      - "5432:5432"
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U user"]
      interval: 10s
      timeout: 5s
      retries: 5
    networks:
      - app-network

  redis:
    image: redis:7-alpine
    command: redis-server --appendonly yes
    volumes:
      - redis_data:/data
    ports:
      - "6379:6379"
    healthcheck:
      test: ["CMD", "redis-cli", "ping"]
      interval: 10s
      timeout: 3s
      retries: 5
    networks:
      - app-network

  bull_board:
    image: node:18-alpine
    working_dir: /app
    command: npm run bull-board
    environment:
      - REDIS_URL=redis://redis:6379
    ports:
      - "3001:3001"
    depends_on:
      - redis
    networks:
      - app-network

volumes:
  postgres_data:
  redis_data:

networks:
  app-network:
    driver: bridge
```

**Development docker-compose.override.yml:**
```yaml
version: '3.8'

services:
  api:
    build:
      target: builder
    command: npm run start:dev
    volumes:
      - ./src:/app/src
      - ./test:/app/test
      - /app/node_modules
    environment:
      - NODE_ENV=development
      - DEBUG=*
```

### Environment Management

**`.env.example`:**
```bash
# Application
NODE_ENV=production
PORT=3000
API_PREFIX=api/v1

# Database
DATABASE_URL=postgresql://user:password@localhost:5432/appdb
DATABASE_HOST=localhost
DATABASE_PORT=5432
DATABASE_USER=user
DATABASE_PASSWORD=password
DATABASE_NAME=appdb
DATABASE_SYNCHRONIZE=false

# Redis
REDIS_URL=redis://localhost:6379
REDIS_HOST=localhost
REDIS_PORT=6379

# JWT Authentication
JWT_SECRET=your-super-secret-jwt-key-change-in-production
JWT_EXPIRATION=3600

# Refresh Token
REFRESH_TOKEN_SECRET=your-refresh-token-secret
REFRESH_TOKEN_EXPIRATION=604800

# CORS
CORS_ORIGIN=http://localhost:3000,https://app.example.com

# Logging
LOG_LEVEL=info
LOG_FORMAT=json

# Rate Limiting
THROTTLE_TTL=60
THROTTLE_LIMIT=10

# File Upload
MAX_FILE_SIZE=10485760

# External APIs
EXTERNAL_API_URL=https://api.example.com
EXTERNAL_API_KEY=your-api-key
```

**Configuration Service (NestJS):**
```typescript
import { Injectable } from '@nestjs/common';
import { ConfigService as NestConfigService } from '@nestjs/config';

@Injectable()
export class ConfigService {
    constructor(private configService: NestConfigService) {}

    get nodeEnv(): string {
        return this.configService.get<string>('NODE_ENV', 'development');
    }

    get isDevelopment(): boolean {
        return this.nodeEnv === 'development';
    }

    get isProduction(): boolean {
        return this.nodeEnv === 'production';
    }

    get port(): number {
        return this.configService.get<number>('PORT', 3000);
    }

    get databaseUrl(): string {
        return this.configService.get<string>('DATABASE_URL');
    }

    get redisUrl(): string {
        return this.configService.get<string>('REDIS_URL');
    }

    get jwtSecret(): string {
        return this.configService.get<string>('JWT_SECRET');
    }

    get jwtExpiration(): number {
        return this.configService.get<number>('JWT_EXPIRATION', 3600);
    }

    get corsOrigins(): string[] {
        const origins = this.configService.get<string>('CORS_ORIGIN', '');
        return origins.split(',').map(origin => origin.trim());
    }
}

// Validation schema
import * as Joi from 'joi';

export const validationSchema = Joi.object({
    NODE_ENV: Joi.string()
        .valid('development', 'production', 'test')
        .default('development'),
    PORT: Joi.number().default(3000),
    DATABASE_URL: Joi.string().required(),
    REDIS_URL: Joi.string().required(),
    JWT_SECRET: Joi.string().required(),
    JWT_EXPIRATION: Joi.number().default(3600),
});

// Module configuration
import { Module } from '@nestjs/common';
import { ConfigModule as NestConfigModule } from '@nestjs/config';

@Module({
    imports: [
        NestConfigModule.forRoot({
            isGlobal: true,
            validationSchema,
            envFilePath: ['.env.local', '.env'],
        }),
    ],
    providers: [ConfigService],
    exports: [ConfigService],
})
export class ConfigModule {}
```

### Database Migrations (TypeORM)

**ormconfig.ts:**
```typescript
import { DataSource, DataSourceOptions } from 'typeorm';
import { config } from 'dotenv';

config();

export const dataSourceOptions: DataSourceOptions = {
    type: 'postgres',
    url: process.env.DATABASE_URL,
    entities: ['dist/**/*.entity.js'],
    migrations: ['dist/migrations/*.js'],
    synchronize: false,
    logging: process.env.NODE_ENV === 'development',
    ssl: process.env.NODE_ENV === 'production' ? { rejectUnauthorized: false } : false,
};

const dataSource = new DataSource(dataSourceOptions);
export default dataSource;
```

**Migration Commands:**
```bash
# Generate migration
npm run migration:generate -- src/migrations/CreateFlightTable

# Create empty migration
npm run migration:create -- src/migrations/AddIndexToFlights

# Run migrations
npm run migration:run

# Revert last migration
npm run migration:revert

# Show migrations
npm run migration:show
```

**package.json scripts:**
```json
{
  "scripts": {
    "migration:generate": "typeorm-ts-node-commonjs migration:generate -d src/config/ormconfig.ts",
    "migration:create": "typeorm-ts-node-commonjs migration:create",
    "migration:run": "typeorm-ts-node-commonjs migration:run -d src/config/ormconfig.ts",
    "migration:revert": "typeorm-ts-node-commonjs migration:revert -d src/config/ormconfig.ts",
    "migration:show": "typeorm-ts-node-commonjs migration:show -d src/config/ormconfig.ts"
  }
}
```

**Example Migration:**
```typescript
import { MigrationInterface, QueryRunner, Table, TableIndex } from 'typeorm';

export class CreateFlightTable1700000000000 implements MigrationInterface {
    public async up(queryRunner: QueryRunner): Promise<void> {
        await queryRunner.createTable(
            new Table({
                name: 'flights',
                columns: [
                    {
                        name: 'id',
                        type: 'int',
                        isPrimary: true,
                        isGenerated: true,
                        generationStrategy: 'increment',
                    },
                    {
                        name: 'carrid',
                        type: 'varchar',
                        length: '3',
                        isNullable: false,
                    },
                    {
                        name: 'connid',
                        type: 'varchar',
                        length: '4',
                        isNullable: false,
                    },
                    {
                        name: 'fldate',
                        type: 'date',
                        isNullable: false,
                    },
                    {
                        name: 'price',
                        type: 'decimal',
                        precision: 10,
                        scale: 2,
                        isNullable: false,
                    },
                    {
                        name: 'currency',
                        type: 'varchar',
                        length: '3',
                        isNullable: false,
                    },
                    {
                        name: 'seats_max',
                        type: 'int',
                        isNullable: false,
                    },
                    {
                        name: 'seats_occupied',
                        type: 'int',
                        default: 0,
                    },
                    {
                        name: 'created_at',
                        type: 'timestamp',
                        default: 'CURRENT_TIMESTAMP',
                    },
                    {
                        name: 'updated_at',
                        type: 'timestamp',
                        default: 'CURRENT_TIMESTAMP',
                        onUpdate: 'CURRENT_TIMESTAMP',
                    },
                ],
            }),
            true,
        );

        await queryRunner.createIndex(
            'flights',
            new TableIndex({
                name: 'IDX_FLIGHTS_CARRID',
                columnNames: ['carrid'],
            }),
        );

        await queryRunner.createIndex(
            'flights',
            new TableIndex({
                name: 'IDX_FLIGHTS_CARRID_CONNID',
                columnNames: ['carrid', 'connid'],
            }),
        );
    }

    public async down(queryRunner: QueryRunner): Promise<void> {
        await queryRunner.dropIndex('flights', 'IDX_FLIGHTS_CARRID_CONNID');
        await queryRunner.dropIndex('flights', 'IDX_FLIGHTS_CARRID');
        await queryRunner.dropTable('flights');
    }
}
```

### CI/CD Configuration

**GitHub Actions (.github/workflows/ci.yml):**
```yaml
name: CI/CD Pipeline

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

jobs:
  lint-and-test:
    runs-on: ubuntu-latest

    services:
      postgres:
        image: postgres:15
        env:
          POSTGRES_USER: test
          POSTGRES_PASSWORD: test
          POSTGRES_DB: testdb
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5
        ports:
          - 5432:5432

      redis:
        image: redis:7
        options: >-
          --health-cmd "redis-cli ping"
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5
        ports:
          - 6379:6379

    strategy:
      matrix:
        node-version: [18.x, 20.x]

    steps:
      - uses: actions/checkout@v3

      - name: Use Node.js ${{ matrix.node-version }}
        uses: actions/setup-node@v3
        with:
          node-version: ${{ matrix.node-version }}
          cache: 'npm'

      - name: Install dependencies
        run: npm ci

      - name: Lint
        run: npm run lint

      - name: Format check
        run: npm run format:check

      - name: Type check
        run: npm run type-check

      - name: Run unit tests
        run: npm run test:cov
        env:
          DATABASE_URL: postgresql://test:test@localhost:5432/testdb
          REDIS_URL: redis://localhost:6379
          JWT_SECRET: test-secret

      - name: Run e2e tests
        run: npm run test:e2e
        env:
          DATABASE_URL: postgresql://test:test@localhost:5432/testdb
          REDIS_URL: redis://localhost:6379
          JWT_SECRET: test-secret

      - name: Upload coverage
        uses: codecov/codecov-action@v3
        with:
          files: ./coverage/lcov.info
          flags: unittests

  build:
    needs: lint-and-test
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'

    steps:
      - uses: actions/checkout@v3

      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v2

      - name: Login to Container Registry
        uses: docker/login-action@v2
        with:
          registry: ghcr.io
          username: ${{ github.actor }}
          password: ${{ secrets.GITHUB_TOKEN }}

      - name: Build and push
        uses: docker/build-push-action@v4
        with:
          context: .
          push: true
          tags: |
            ghcr.io/${{ github.repository }}:latest
            ghcr.io/${{ github.repository }}:${{ github.sha }}
          cache-from: type=gha
          cache-to: type=gha,mode=max

  deploy:
    needs: build
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'
    environment: production

    steps:
      - name: Deploy to production
        run: |
          echo "Deploying to production..."
          # Add deployment steps here (e.g., kubectl, terraform, etc.)
```

**GitLab CI (.gitlab-ci.yml):**
```yaml
stages:
  - test
  - build
  - deploy

variables:
  POSTGRES_DB: testdb
  POSTGRES_USER: test
  POSTGRES_PASSWORD: test
  DATABASE_URL: postgresql://test:test@postgres:5432/testdb
  REDIS_URL: redis://redis:6379

test:
  stage: test
  image: node:18-alpine
  services:
    - postgres:15-alpine
    - redis:7-alpine
  before_script:
    - npm ci
  script:
    - npm run lint
    - npm run test:cov
    - npm run test:e2e
  coverage: '/Lines\s*:\s*(\d+\.\d+)%/'
  artifacts:
    reports:
      coverage_report:
        coverage_format: cobertura
        path: coverage/cobertura-coverage.xml

build:
  stage: build
  image: docker:latest
  services:
    - docker:dind
  before_script:
    - docker login -u $CI_REGISTRY_USER -p $CI_REGISTRY_PASSWORD $CI_REGISTRY
  script:
    - docker build -t $CI_REGISTRY_IMAGE:$CI_COMMIT_SHA .
    - docker tag $CI_REGISTRY_IMAGE:$CI_COMMIT_SHA $CI_REGISTRY_IMAGE:latest
    - docker push $CI_REGISTRY_IMAGE:$CI_COMMIT_SHA
    - docker push $CI_REGISTRY_IMAGE:latest
  only:
    - main

deploy:
  stage: deploy
  image: alpine:latest
  before_script:
    - apk add --no-cache curl
  script:
    - echo "Deploying to production..."
    # Add deployment commands
  only:
    - main
  when: manual
```

### Logging Configuration

**Winston Logger Setup:**
```typescript
import { WinstonModule } from 'nest-winston';
import * as winston from 'winston';
import 'winston-daily-rotate-file';

const logFormat = winston.format.combine(
    winston.format.timestamp({ format: 'YYYY-MM-DD HH:mm:ss' }),
    winston.format.errors({ stack: true }),
    winston.format.splat(),
    winston.format.json(),
);

const dailyRotateFileTransport = new winston.transports.DailyRotateFile({
    filename: 'logs/application-%DATE%.log',
    datePattern: 'YYYY-MM-DD',
    zippedArchive: true,
    maxSize: '20m',
    maxFiles: '14d',
    format: logFormat,
});

const errorFileTransport = new winston.transports.DailyRotateFile({
    filename: 'logs/error-%DATE%.log',
    datePattern: 'YYYY-MM-DD',
    zippedArchive: true,
    maxSize: '20m',
    maxFiles: '30d',
    level: 'error',
    format: logFormat,
});

export const winstonConfig = {
    transports: [
        new winston.transports.Console({
            format: winston.format.combine(
                winston.format.colorize(),
                winston.format.simple(),
            ),
        }),
        dailyRotateFileTransport,
        errorFileTransport,
    ],
};

// Use in main.ts
import { Logger } from '@nestjs/common';

async function bootstrap() {
    const app = await NestFactory.create(AppModule, {
        logger: WinstonModule.createLogger(winstonConfig),
    });

    // Global logging interceptor
    app.useGlobalInterceptors(new LoggingInterceptor());

    await app.listen(3000);
}
```

**Custom Logger Service:**
```typescript
import { Injectable, LoggerService } from '@nestjs/common';
import { Logger } from 'winston';
import { WINSTON_MODULE_PROVIDER } from 'nest-winston';
import { Inject } from '@nestjs/common';

@Injectable()
export class CustomLogger implements LoggerService {
    constructor(
        @Inject(WINSTON_MODULE_PROVIDER) private readonly logger: Logger,
    ) {}

    log(message: string, context?: string) {
        this.logger.info(message, { context });
    }

    error(message: string, trace?: string, context?: string) {
        this.logger.error(message, { trace, context });
    }

    warn(message: string, context?: string) {
        this.logger.warn(message, { context });
    }

    debug(message: string, context?: string) {
        this.logger.debug(message, { context });
    }

    verbose(message: string, context?: string) {
        this.logger.verbose(message, { context });
    }
}
```

### Health Checks and Monitoring

**Health Check Module:**
```typescript
import { Module } from '@nestjs/common';
import { TerminusModule } from '@nestjs/terminus';
import { HttpModule } from '@nestjs/axios';
import { HealthController } from './health.controller';

@Module({
    imports: [TerminusModule, HttpModule],
    controllers: [HealthController],
})
export class HealthModule {}
```

**Health Controller:**
```typescript
import { Controller, Get } from '@nestjs/common';
import {
    HealthCheck,
    HealthCheckService,
    TypeOrmHealthIndicator,
    MemoryHealthIndicator,
    DiskHealthIndicator,
} from '@nestjs/terminus';
import { InjectRedis } from '@liaoliaots/nestjs-redis';
import Redis from 'ioredis';

@Controller('health')
export class HealthController {
    constructor(
        private health: HealthCheckService,
        private db: TypeOrmHealthIndicator,
        private memory: MemoryHealthIndicator,
        private disk: DiskHealthIndicator,
        @InjectRedis() private redis: Redis,
    ) {}

    @Get()
    @HealthCheck()
    check() {
        return this.health.check([
            // Database check
            () => this.db.pingCheck('database'),

            // Redis check
            async () => {
                const isHealthy = (await this.redis.ping()) === 'PONG';
                return {
                    redis: {
                        status: isHealthy ? 'up' : 'down',
                    },
                };
            },

            // Memory heap check (should not exceed 300MB)
            () => this.memory.checkHeap('memory_heap', 300 * 1024 * 1024),

            // Memory RSS check (should not exceed 500MB)
            () => this.memory.checkRSS('memory_rss', 500 * 1024 * 1024),

            // Disk storage check (should have at least 50% free)
            () =>
                this.disk.checkStorage('storage', {
                    path: '/',
                    thresholdPercent: 0.5,
                }),
        ]);
    }

    @Get('ready')
    @HealthCheck()
    ready() {
        return this.health.check([
            () => this.db.pingCheck('database'),
            async () => {
                const isHealthy = (await this.redis.ping()) === 'PONG';
                return {
                    redis: {
                        status: isHealthy ? 'up' : 'down',
                    },
                };
            },
        ]);
    }

    @Get('live')
    live() {
        return { status: 'ok' };
    }
}
```

### Performance Optimization

**Compression Middleware:**
```typescript
import { NestFactory } from '@nestjs/core';
import * as compression from 'compression';
import helmet from 'helmet';

async function bootstrap() {
    const app = await NestFactory.create(AppModule);

    // Enable compression
    app.use(compression());

    // Security headers
    app.use(helmet());

    await app.listen(3000);
}
```

**Caching:**
```typescript
import { Module } from '@nestjs/common';
import { CacheModule } from '@nestjs/cache-manager';
import * as redisStore from 'cache-manager-redis-store';

@Module({
    imports: [
        CacheModule.register({
            isGlobal: true,
            store: redisStore,
            host: process.env.REDIS_HOST,
            port: process.env.REDIS_PORT,
            ttl: 300, // 5 minutes default
        }),
    ],
})
export class AppModule {}

// Usage in controller
import { CacheInterceptor, CacheTTL } from '@nestjs/common';

@Controller('flights')
@UseInterceptors(CacheInterceptor)
export class FlightController {
    @Get()
    @CacheTTL(600) // Cache for 10 minutes
    async findAll() {
        return this.flightService.findAll();
    }
}
```

### Kubernetes Deployment

**deployment.yaml:**
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: nestjs-app
  labels:
    app: nestjs-app
spec:
  replicas: 3
  selector:
    matchLabels:
      app: nestjs-app
  template:
    metadata:
      labels:
        app: nestjs-app
    spec:
      containers:
        - name: nestjs-app
          image: ghcr.io/your-org/your-app:latest
          ports:
            - containerPort: 3000
          env:
            - name: NODE_ENV
              value: "production"
            - name: DATABASE_URL
              valueFrom:
                secretKeyRef:
                  name: app-secrets
                  key: database-url
            - name: REDIS_URL
              valueFrom:
                secretKeyRef:
                  name: app-secrets
                  key: redis-url
            - name: JWT_SECRET
              valueFrom:
                secretKeyRef:
                  name: app-secrets
                  key: jwt-secret
          livenessProbe:
            httpGet:
              path: /health/live
              port: 3000
            initialDelaySeconds: 30
            periodSeconds: 10
          readinessProbe:
            httpGet:
              path: /health/ready
              port: 3000
            initialDelaySeconds: 10
            periodSeconds: 5
          resources:
            requests:
              memory: "256Mi"
              cpu: "250m"
            limits:
              memory: "512Mi"
              cpu: "500m"
---
apiVersion: v1
kind: Service
metadata:
  name: nestjs-app-service
spec:
  selector:
    app: nestjs-app
  ports:
    - protocol: TCP
      port: 80
      targetPort: 3000
  type: LoadBalancer
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
