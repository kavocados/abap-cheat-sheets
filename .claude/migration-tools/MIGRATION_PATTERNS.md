# ABAP to Python/JavaScript Migration Patterns

Comprehensive guide to migrating common ABAP patterns to modern Python and JavaScript.

## Table of Contents

1. [Data Types](#data-types)
2. [Database Operations](#database-operations)
3. [Internal Tables](#internal-tables)
4. [Function Modules](#function-modules)
5. [Object-Oriented Patterns](#object-oriented-patterns)
6. [Transaction Handling](#transaction-handling)
7. [Error Handling](#error-handling)
8. [Authorization](#authorization)
9. [Background Processing](#background-processing)
10. [Integration Patterns](#integration-patterns)

---

## Data Types

### Basic Types

| ABAP | Python | TypeScript | Notes |
|------|--------|------------|-------|
| `DATA lv_num TYPE i` | `num: int = 0` | `let num: number = 0` | Integer |
| `DATA lv_text TYPE string` | `text: str = ""` | `let text: string = ""` | String |
| `DATA lv_amount TYPE p DECIMALS 2` | `from decimal import Decimal`<br>`amount = Decimal('0.00')` | `// Use decimal.js`<br>`let amount: Decimal` | Currency/decimals |
| `DATA lv_date TYPE d` | `from datetime import date`<br>`date_val = date.today()` | `let dateVal: Date` | Date (YYYYMMDD) |
| `DATA lv_flag TYPE abap_bool` | `flag: bool = False` | `let flag: boolean = false` | Boolean |

### Structures

**ABAP:**
```abap
TYPES: BEGIN OF ty_customer,
         customer_id TYPE kunnr,
         name        TYPE string,
         amount      TYPE p DECIMALS 2,
       END OF ty_customer.

DATA ls_customer TYPE ty_customer.
```

**Python (dataclass):**
```python
from dataclasses import dataclass
from decimal import Decimal

@dataclass
class Customer:
    customer_id: str
    name: str
    amount: Decimal
```

**TypeScript (interface):**
```typescript
interface Customer {
  customerId: string;
  name: string;
  amount: number;
}

const customer: Customer = {
  customerId: '0001',
  name: 'ACME Corp',
  amount: 1000.00
};
```

---

## Database Operations

### SELECT - Single Record

**ABAP:**
```abap
SELECT SINGLE * FROM kna1
  INTO @DATA(ls_customer)
  WHERE kunnr = @lv_customer_id.

IF sy-subrc <> 0.
  " Not found
ENDIF.
```

**Python (SQLAlchemy):**
```python
from sqlalchemy.orm import Session

customer = db.query(Customer)\
    .filter(Customer.customer_id == customer_id)\
    .first()

if customer is None:
    # Not found
    pass
```

**TypeScript (Prisma):**
```typescript
const customer = await prisma.customer.findUnique({
  where: { customerId: customerId }
});

if (!customer) {
  // Not found
}
```

### SELECT - Multiple Records

**ABAP:**
```abap
SELECT * FROM kna1
  INTO TABLE @DATA(lt_customers)
  WHERE land1 = @lv_country
  ORDER BY name1.
```

**Python (SQLAlchemy):**
```python
customers = db.query(Customer)\
    .filter(Customer.country == country)\
    .order_by(Customer.name)\
    .all()
```

**TypeScript (Prisma):**
```typescript
const customers = await prisma.customer.findMany({
  where: { country: country },
  orderBy: { name: 'asc' }
});
```

### INSERT

**ABAP:**
```abap
DATA ls_customer TYPE kna1.
ls_customer-kunnr = '0001'.
ls_customer-name1 = 'ACME Corp'.

INSERT kna1 FROM @ls_customer.
```

**Python (SQLAlchemy):**
```python
customer = Customer(
    customer_id='0001',
    name='ACME Corp'
)
db.add(customer)
db.commit()
```

**TypeScript (Prisma):**
```typescript
const customer = await prisma.customer.create({
  data: {
    customerId: '0001',
    name: 'ACME Corp'
  }
});
```

### UPDATE

**ABAP:**
```abap
UPDATE kna1
  SET name1 = @lv_new_name
  WHERE kunnr = @lv_customer_id.
```

**Python (SQLAlchemy):**
```python
db.query(Customer)\
    .filter(Customer.customer_id == customer_id)\
    .update({'name': new_name})
db.commit()
```

**TypeScript (Prisma):**
```typescript
await prisma.customer.update({
  where: { customerId: customerId },
  data: { name: newName }
});
```

### DELETE

**ABAP:**
```abap
DELETE FROM kna1 WHERE kunnr = @lv_customer_id.
```

**Python (SQLAlchemy):**
```python
db.query(Customer)\
    .filter(Customer.customer_id == customer_id)\
    .delete()
db.commit()
```

**TypeScript (Prisma):**
```typescript
await prisma.customer.delete({
  where: { customerId: customerId }
});
```

---

## Internal Tables

### Filter Operations

**ABAP:**
```abap
DATA lt_filtered TYPE TABLE OF ty_item.
lt_filtered = FILTER #( lt_items WHERE price > 100 ).
```

**Python:**
```python
filtered = [item for item in items if item.price > 100]
# Or with filter()
filtered = list(filter(lambda x: x.price > 100, items))
```

**TypeScript:**
```typescript
const filtered = items.filter(item => item.price > 100);
```

### Sort Operations

**ABAP:**
```abap
SORT lt_items BY price DESCENDING.
```

**Python:**
```python
items.sort(key=lambda x: x.price, reverse=True)
# Or sorted() for new list
sorted_items = sorted(items, key=lambda x: x.price, reverse=True)
```

**TypeScript:**
```typescript
items.sort((a, b) => b.price - a.price);
```

### Map/Transform

**ABAP:**
```abap
DATA lt_names TYPE TABLE OF string.
lt_names = VALUE #( FOR item IN lt_items ( item-name ) ).
```

**Python:**
```python
names = [item.name for item in items]
# Or with map()
names = list(map(lambda x: x.name, items))
```

**TypeScript:**
```typescript
const names = items.map(item => item.name);
```

### Reduce/Aggregate

**ABAP:**
```abap
DATA lv_total TYPE p.
lv_total = REDUCE p( INIT sum = 0
                     FOR item IN lt_items
                     NEXT sum = sum + item-price ).
```

**Python:**
```python
from functools import reduce

total = reduce(lambda sum, item: sum + item.price, items, 0)
# Or simply
total = sum(item.price for item in items)
```

**TypeScript:**
```typescript
const total = items.reduce((sum, item) => sum + item.price, 0);
```

### Group By

**ABAP:**
```abap
LOOP AT lt_items INTO DATA(ls_item)
     GROUP BY ( category = ls_item-category )
     INTO DATA(ls_group).

  WRITE: / ls_group-category.

  LOOP AT GROUP ls_group INTO DATA(ls_member).
    WRITE: / ls_member-name.
  ENDLOOP.
ENDLOOP.
```

**Python (using itertools):**
```python
from itertools import groupby

items_sorted = sorted(items, key=lambda x: x.category)
for category, group in groupby(items_sorted, key=lambda x: x.category):
    print(category)
    for item in group:
        print(f"  {item.name}")
```

**Python (using pandas):**
```python
import pandas as pd

df = pd.DataFrame([vars(item) for item in items])
grouped = df.groupby('category')

for category, group_df in grouped:
    print(category)
    for _, row in group_df.iterrows():
        print(f"  {row['name']}")
```

**TypeScript:**
```typescript
const grouped = items.reduce((acc, item) => {
  const category = item.category;
  if (!acc[category]) {
    acc[category] = [];
  }
  acc[category].push(item);
  return acc;
}, {} as Record<string, Item[]>);

for (const [category, items] of Object.entries(grouped)) {
  console.log(category);
  items.forEach(item => console.log(`  ${item.name}`));
}
```

---

## Function Modules

### ABAP Function Module

**ABAP:**
```abap
FUNCTION z_calculate_discount.
  IMPORTING
    iv_amount       TYPE p
    iv_customer_type TYPE string
  EXPORTING
    ev_discount     TYPE p
    ev_final_amount TYPE p
  EXCEPTIONS
    invalid_amount
    invalid_customer_type.

  IF iv_amount <= 0.
    RAISE invalid_amount.
  ENDIF.

  CASE iv_customer_type.
    WHEN 'VIP'.
      ev_discount = iv_amount * '0.20'.
    WHEN 'REGULAR'.
      ev_discount = iv_amount * '0.10'.
    WHEN OTHERS.
      RAISE invalid_customer_type.
  ENDCASE.

  ev_final_amount = iv_amount - ev_discount.
ENDFUNCTION.
```

**Python (Function):**
```python
from decimal import Decimal
from typing import Tuple

class InvalidAmountError(Exception):
    pass

class InvalidCustomerTypeError(Exception):
    pass

def calculate_discount(
    amount: Decimal,
    customer_type: str
) -> Tuple[Decimal, Decimal]:
    """
    Calculate discount based on customer type.

    Args:
        amount: Purchase amount
        customer_type: Type of customer (VIP, REGULAR)

    Returns:
        Tuple of (discount, final_amount)

    Raises:
        InvalidAmountError: If amount <= 0
        InvalidCustomerTypeError: If customer type not recognized
    """
    if amount <= 0:
        raise InvalidAmountError("Amount must be positive")

    discount_rates = {
        'VIP': Decimal('0.20'),
        'REGULAR': Decimal('0.10')
    }

    if customer_type not in discount_rates:
        raise InvalidCustomerTypeError(f"Unknown customer type: {customer_type}")

    discount = amount * discount_rates[customer_type]
    final_amount = amount - discount

    return discount, final_amount
```

**Python (FastAPI Endpoint):**
```python
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel, Field
from decimal import Decimal

app = FastAPI()

class DiscountRequest(BaseModel):
    amount: Decimal = Field(gt=0)
    customer_type: str = Field(pattern="^(VIP|REGULAR)$")

class DiscountResponse(BaseModel):
    discount: Decimal
    final_amount: Decimal

@app.post("/calculate-discount", response_model=DiscountResponse)
async def calculate_discount(request: DiscountRequest):
    """Calculate discount based on customer type"""
    discount_rates = {
        'VIP': Decimal('0.20'),
        'REGULAR': Decimal('0.10')
    }

    discount = request.amount * discount_rates[request.customer_type]
    final_amount = request.amount - discount

    return DiscountResponse(
        discount=discount,
        final_amount=final_amount
    )
```

**TypeScript (NestJS):**
```typescript
import { Injectable, BadRequestException } from '@nestjs/common';

export class DiscountDto {
  amount: number;
  customerType: 'VIP' | 'REGULAR';
}

export class DiscountResultDto {
  discount: number;
  finalAmount: number;
}

@Injectable()
export class DiscountService {
  calculateDiscount(dto: DiscountDto): DiscountResultDto {
    if (dto.amount <= 0) {
      throw new BadRequestException('Amount must be positive');
    }

    const discountRates = {
      'VIP': 0.20,
      'REGULAR': 0.10
    };

    if (!(dto.customerType in discountRates)) {
      throw new BadRequestException('Invalid customer type');
    }

    const discount = dto.amount * discountRates[dto.customerType];
    const finalAmount = dto.amount - discount;

    return { discount, finalAmount };
  }
}

// Controller
@Controller('discount')
export class DiscountController {
  constructor(private discountService: DiscountService) {}

  @Post('calculate')
  calculate(@Body() dto: DiscountDto): DiscountResultDto {
    return this.discountService.calculateDiscount(dto);
  }
}
```

---

## Object-Oriented Patterns

### Class Definition

**ABAP:**
```abap
CLASS zcl_invoice DEFINITION PUBLIC FINAL.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_invoice_id TYPE string.

    METHODS calculate_total
      RETURNING VALUE(rv_total) TYPE p.

    METHODS add_line_item
      IMPORTING is_item TYPE ty_line_item.

  PRIVATE SECTION.
    DATA mv_invoice_id TYPE string.
    DATA mt_items TYPE TABLE OF ty_line_item.
ENDCLASS.

CLASS zcl_invoice IMPLEMENTATION.
  METHOD constructor.
    mv_invoice_id = iv_invoice_id.
  ENDMETHOD.

  METHOD calculate_total.
    rv_total = REDUCE p( INIT sum = 0
                         FOR item IN mt_items
                         NEXT sum = sum + item-amount ).
  ENDMETHOD.

  METHOD add_line_item.
    APPEND is_item TO mt_items.
  ENDMETHOD.
ENDCLASS.
```

**Python:**
```python
from dataclasses import dataclass, field
from decimal import Decimal
from typing import List

@dataclass
class LineItem:
    description: str
    amount: Decimal

class Invoice:
    """Invoice class for managing invoice data"""

    def __init__(self, invoice_id: str):
        self.invoice_id = invoice_id
        self.items: List[LineItem] = []

    def calculate_total(self) -> Decimal:
        """Calculate total amount of all line items"""
        return sum(item.amount for item in self.items)

    def add_line_item(self, item: LineItem) -> None:
        """Add a line item to the invoice"""
        self.items.append(item)
```

**TypeScript:**
```typescript
interface LineItem {
  description: string;
  amount: number;
}

class Invoice {
  private invoiceId: string;
  private items: LineItem[] = [];

  constructor(invoiceId: string) {
    this.invoiceId = invoiceId;
  }

  calculateTotal(): number {
    return this.items.reduce((sum, item) => sum + item.amount, 0);
  }

  addLineItem(item: LineItem): void {
    this.items.push(item);
  }

  getInvoiceId(): string {
    return this.invoiceId;
  }

  getItems(): readonly LineItem[] {
    return this.items;
  }
}
```

---

## Transaction Handling

### ABAP Transaction Pattern

**ABAP:**
```abap
" Start transaction
INSERT orders FROM TABLE @lt_orders.
INSERT order_items FROM TABLE @lt_items.

IF sy-subrc = 0.
  COMMIT WORK AND WAIT.
ELSE.
  ROLLBACK WORK.
ENDIF.
```

**Python (SQLAlchemy):**
```python
from sqlalchemy.orm import Session

def create_order_with_items(db: Session, order: Order, items: List[OrderItem]):
    try:
        # Start transaction (implicit)
        db.add(order)
        db.add_all(items)

        # Commit
        db.commit()
    except Exception as e:
        # Rollback on error
        db.rollback()
        raise
```

**TypeScript (Prisma):**
```typescript
async function createOrderWithItems(
  order: Order,
  items: OrderItem[]
): Promise<void> {
  try {
    // Transaction with Prisma
    await prisma.$transaction(async (tx) => {
      const createdOrder = await tx.order.create({
        data: order
      });

      await tx.orderItem.createMany({
        data: items.map(item => ({
          ...item,
          orderId: createdOrder.id
        }))
      });
    });
  } catch (error) {
    console.error('Transaction failed:', error);
    throw error;
  }
}
```

**TypeScript (TypeORM):**
```typescript
import { DataSource } from 'typeorm';

async function createOrderWithItems(
  dataSource: DataSource,
  order: Order,
  items: OrderItem[]
): Promise<void> {
  const queryRunner = dataSource.createQueryRunner();

  await queryRunner.connect();
  await queryRunner.startTransaction();

  try {
    const savedOrder = await queryRunner.manager.save(order);

    items.forEach(item => {
      item.order = savedOrder;
    });

    await queryRunner.manager.save(items);

    await queryRunner.commitTransaction();
  } catch (err) {
    await queryRunner.rollbackTransaction();
    throw err;
  } finally {
    await queryRunner.release();
  }
}
```

---

## Error Handling

### Exception Classes

**ABAP:**
```abap
CLASS zcx_business_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING textid LIKE textid OPTIONAL
                previous LIKE previous OPTIONAL
                iv_message TYPE string OPTIONAL.
ENDCLASS.

" Usage
TRY.
    " Some operation
    IF error_condition.
      RAISE EXCEPTION TYPE zcx_business_error
        EXPORTING iv_message = 'Business rule violation'.
    ENDIF.
  CATCH zcx_business_error INTO DATA(lx_error).
    " Handle error
    WRITE: / lx_error->get_text( ).
ENDTRY.
```

**Python:**
```python
class BusinessError(Exception):
    """Custom business error exception"""

    def __init__(self, message: str, code: str = None):
        self.message = message
        self.code = code
        super().__init__(self.message)

# Usage
try:
    if error_condition:
        raise BusinessError("Business rule violation", code="BUS001")
except BusinessError as e:
    print(f"Error: {e.message} (Code: {e.code})")
```

**TypeScript:**
```typescript
class BusinessError extends Error {
  code?: string;

  constructor(message: string, code?: string) {
    super(message);
    this.name = 'BusinessError';
    this.code = code;
  }
}

// Usage
try {
  if (errorCondition) {
    throw new BusinessError('Business rule violation', 'BUS001');
  }
} catch (error) {
  if (error instanceof BusinessError) {
    console.error(`Error: ${error.message} (Code: ${error.code})`);
  }
}
```

---

## Authorization

### ABAP Authority Check

**ABAP:**
```abap
AUTHORITY-CHECK OBJECT 'Z_ORDER'
  ID 'ACTVT' FIELD '02'  " Change
  ID 'BUKRS' FIELD lv_company_code.

IF sy-subrc <> 0.
  " No authorization
  MESSAGE e001(z_messages).
ENDIF.
```

**Python (Custom RBAC):**
```python
from fastapi import Depends, HTTPException, status
from typing import List

def check_permission(required_permission: str):
    """Decorator for permission checks"""
    def permission_checker(current_user: User = Depends(get_current_user)):
        if required_permission not in current_user.permissions:
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail="Insufficient permissions"
            )
        return current_user
    return permission_checker

# Usage
@app.put("/orders/{order_id}")
async def update_order(
    order_id: int,
    order: OrderUpdate,
    user: User = Depends(check_permission("order:write"))
):
    # Update order logic
    pass
```

**TypeScript (NestJS with Guards):**
```typescript
import { Injectable, CanActivate, ExecutionContext } from '@nestjs/common';
import { Reflector } from '@nestjs/core';

@Injectable()
export class PermissionsGuard implements CanActivate {
  constructor(private reflector: Reflector) {}

  canActivate(context: ExecutionContext): boolean {
    const requiredPermissions = this.reflector.get<string[]>(
      'permissions',
      context.getHandler()
    );

    if (!requiredPermissions) {
      return true;
    }

    const request = context.switchToHttp().getRequest();
    const user = request.user;

    return requiredPermissions.every(permission =>
      user.permissions.includes(permission)
    );
  }
}

// Usage
@Put(':id')
@Permissions('order:write')
@UseGuards(PermissionsGuard)
async updateOrder(
  @Param('id') id: number,
  @Body() updateDto: UpdateOrderDto
): Promise<Order> {
  return this.orderService.update(id, updateDto);
}
```

---

## Background Processing

### ABAP Batch Job

**ABAP:**
```abap
REPORT z_batch_process.

START-OF-SELECTION.
  " Process large dataset
  SELECT * FROM large_table
    INTO TABLE @DATA(lt_data)
    PACKAGE SIZE 1000.

    " Process package
    LOOP AT lt_data INTO DATA(ls_data).
      " Processing logic
    ENDLOOP.

    COMMIT WORK.
  ENDSELECT.
```

**Python (Celery):**
```python
from celery import Celery
from sqlalchemy.orm import Session

app = Celery('tasks', broker='redis://localhost:6379')

@app.task
def process_large_dataset():
    """Background task for processing large dataset"""
    batch_size = 1000
    offset = 0

    with Session() as db:
        while True:
            # Fetch batch
            items = db.query(LargeTable)\
                .offset(offset)\
                .limit(batch_size)\
                .all()

            if not items:
                break

            # Process batch
            for item in items:
                # Processing logic
                pass

            db.commit()
            offset += batch_size

# Schedule task
process_large_dataset.delay()
```

**TypeScript (Bull Queue):**
```typescript
import Queue from 'bull';
import { PrismaClient } from '@prisma/client';

const prisma = new PrismaClient();
const processQueue = new Queue('large-dataset-processing', {
  redis: { host: 'localhost', port: 6379 }
});

interface ProcessJob {
  batchSize: number;
}

processQueue.process(async (job, done) => {
  const { batchSize = 1000 } = job.data as ProcessJob;
  let offset = 0;

  try {
    while (true) {
      const items = await prisma.largeTable.findMany({
        skip: offset,
        take: batchSize
      });

      if (items.length === 0) {
        break;
      }

      // Process batch
      for (const item of items) {
        // Processing logic
        job.progress((offset / totalCount) * 100);
      }

      offset += batchSize;
    }

    done();
  } catch (error) {
    done(error as Error);
  }
});

// Queue a job
processQueue.add({ batchSize: 1000 });
```

---

## Integration Patterns

### RFC/BAPI Call Migration

**ABAP (Calling RFC):**
```abap
CALL FUNCTION 'BAPI_CUSTOMER_GETDETAIL'
  DESTINATION 'RFC_DEST'
  EXPORTING
    customerno = lv_customer_id
  IMPORTING
    customeraddress = ls_address
  EXCEPTIONS
    communication_failure = 1
    system_failure = 2.
```

**Python (REST API Call):**
```python
import requests
from typing import Optional

def get_customer_details(customer_id: str) -> Optional[dict]:
    """Call customer service API"""
    response = requests.get(
        f"https://api.example.com/customers/{customer_id}",
        headers={"Authorization": f"Bearer {token}"},
        timeout=30
    )

    if response.status_code == 200:
        return response.json()
    elif response.status_code == 404:
        return None
    else:
        response.raise_for_status()
```

**TypeScript (Axios):**
```typescript
import axios from 'axios';

interface CustomerAddress {
  street: string;
  city: string;
  country: string;
}

async function getCustomerDetails(
  customerId: string
): Promise<CustomerAddress | null> {
  try {
    const response = await axios.get(
      `https://api.example.com/customers/${customerId}`,
      {
        headers: { Authorization: `Bearer ${token}` },
        timeout: 30000
      }
    );

    return response.data;
  } catch (error) {
    if (axios.isAxiosError(error) && error.response?.status === 404) {
      return null;
    }
    throw error;
  }
}
```

---

## Summary

This migration patterns guide covers the most common ABAP patterns and their equivalents in Python and JavaScript/TypeScript. Key principles:

1. **Type Safety**: Use type hints in Python and TypeScript for better code quality
2. **Error Handling**: Modern exception handling with custom error classes
3. **Database Access**: Use ORMs (SQLAlchemy, Prisma, TypeORM) instead of raw SQL
4. **API Design**: REST APIs with proper HTTP methods and status codes
5. **Authentication**: JWT/OAuth2 instead of SAP authorization objects
6. **Background Processing**: Celery/Bull for async tasks
7. **Testing**: pytest/Jest for comprehensive test coverage
8. **Documentation**: OpenAPI/Swagger for API documentation

For complex migrations, consider:
- Strangler Fig pattern for gradual migration
- API Gateway for routing between old and new systems
- Event-driven architecture for decoupling
- Comprehensive integration testing
