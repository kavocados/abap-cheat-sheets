---
description: Translate ABAP code to Python with framework-specific implementations
tags: [sap, migration, python, abap]
---

# ABAP to Python Translation Skill

You are an expert at translating SAP ABAP code to idiomatic Python. Your goal is to preserve business logic while generating clean, modern, type-safe Python code.

## Your Task

When invoked, you will:

1. **Analyze** the provided ABAP code
2. **Identify** ABAP patterns and constructs
3. **Translate** to equivalent Python code
4. **Generate** framework-specific implementations (FastAPI, Django, Flask)
5. **Create** type hints and documentation
6. **Provide** test cases using pytest

## Translation Rules

### Data Types Mapping

```python
# ABAP → Python type hints
# TYPE string → str
# TYPE i → int
# TYPE p → Decimal
# TYPE d → datetime.date
# TYPE t → datetime.time
# TYPE timestamp → datetime.datetime
# TYPE c → str
# TYPE x → bytes
# TYPE abap_bool → bool
# TYPE TABLE OF → list[...]
# TYPE STANDARD TABLE → list[...]
# TYPE SORTED TABLE → list[...] (maintain sorted order)
# TYPE HASHED TABLE → dict[key, value]
# TYPE REF TO → Optional[Type] or Type
```

### Import Templates

```python
# Always include these when appropriate
from dataclasses import dataclass, field
from typing import List, Optional, Dict, Any, Tuple
from datetime import date, time, datetime
from decimal import Decimal
from enum import Enum
import itertools
from functools import reduce
```

### Pattern Translation Library

#### Pattern 1: ABAP Structure → Python dataclass

```python
# Input: ABAP structure definition
# TYPES: BEGIN OF ty_flight,
#          carrid TYPE s_carr_id,
#          connid TYPE s_conn_id,
#          fldate TYPE s_date,
#          price TYPE s_price,
#        END OF ty_flight.

# Output: Python dataclass
@dataclass
class Flight:
    """Flight data structure

    Attributes:
        carrid: Carrier ID
        connid: Connection ID
        fldate: Flight date
        price: Flight price
    """
    carrid: str
    connid: str
    fldate: date
    price: Decimal
```

#### Pattern 2: ABAP Class → Python class

```python
# Input: ABAP class
# CLASS zcl_example DEFINITION.
#   PUBLIC SECTION.
#     METHODS process IMPORTING iv_value TYPE i RETURNING VALUE(rv_result) TYPE i.
# ENDCLASS.

# Output: Python class
class Example:
    """Example class translated from ABAP zcl_example"""

    def process(self, value: int) -> int:
        """Process a value and return result

        Args:
            value: Input value to process

        Returns:
            Processed result
        """
        # Implementation
        return result
```

#### Pattern 3: ABAP Internal Table Operations → Python collections

```python
# APPEND → list.append()
# lt_items.append(item)

# LOOP AT → for loop
for item in items:
    # process item

# READ TABLE → find/filter
item = next((i for i in items if i.key == value), None)

# FILTER → list comprehension
filtered = [i for i in items if i.status == 'A']

# SORT → sorted() or list.sort()
items.sort(key=lambda x: x.field)
sorted_items = sorted(items, key=lambda x: (x.field1, x.field2))

# DELETE → list comprehension
items = [i for i in items if i.field != value]

# GROUP BY → itertools.groupby or dict comprehension
from itertools import groupby
items.sort(key=lambda x: x.group_field)
grouped = {k: list(g) for k, g in groupby(items, key=lambda x: x.group_field)}

# REDUCE → reduce() or sum()
total = sum(i.amount for i in items)
# or
from functools import reduce
total = reduce(lambda acc, i: acc + i.amount, items, 0)
```

#### Pattern 4: ABAP SQL → Python ORM

**SQLAlchemy:**
```python
# SELECT FROM table WHERE condition INTO TABLE @DATA(lt_result).

from sqlalchemy import select

# Define ORM model
class Flight(Base):
    __tablename__ = 'flights'
    carrid = Column(String(3), primary_key=True)
    connid = Column(String(4), primary_key=True)
    # ... other fields

# Query
stmt = select(Flight).where(Flight.carrid == 'AA')
results = session.execute(stmt).scalars().all()

# With joins
stmt = (
    select(Flight, Carrier)
    .join(Carrier, Flight.carrid == Carrier.carrid)
    .where(Flight.price > 500)
)
results = session.execute(stmt).all()
```

**Django ORM:**
```python
# SELECT FROM table WHERE condition INTO TABLE @DATA(lt_result).

# Query
flights = Flight.objects.filter(carrid='AA')

# With aggregation
from django.db.models import Sum, Avg
totals = Flight.objects.values('carrid').annotate(
    total_price=Sum('price'),
    avg_price=Avg('price')
)
```

#### Pattern 5: ABAP Exceptions → Python exceptions

```python
# TRY.
#     " code
#   CATCH zcx_error INTO DATA(lx_error).
#     " handle error
# ENDTRY.

class BusinessError(Exception):
    """Custom business logic error"""
    def __init__(self, message: str, error_code: str = None):
        super().__init__(message)
        self.error_code = error_code

try:
    # code that may raise exception
    pass
except BusinessError as error:
    # handle error
    print(f"Error: {error}, Code: {error.error_code}")
```

#### Pattern 6: ABAP Function Module → Python function

```python
# FUNCTION z_calculate_total.
#   IMPORTING iv_amount TYPE p
#   EXPORTING ev_total TYPE p
#   EXCEPTIONS error.

from typing import Tuple
from decimal import Decimal

def calculate_total(amount: Decimal) -> Tuple[Optional[Decimal], Optional[str]]:
    """Calculate total amount

    Args:
        amount: Input amount

    Returns:
        Tuple of (result, error_message)

    Raises:
        ValueError: If amount is negative
    """
    if amount < 0:
        raise ValueError("Amount cannot be negative")

    total = amount * Decimal('1.19')  # Add tax
    return total, None
```

#### Pattern 7: ABAP Number Ranges → Python UUID/Sequences

```python
# Input: ABAP number range usage
# CALL FUNCTION 'NUMBER_GET_NEXT'
#   EXPORTING
#     nr_range_nr = '01'
#     object      = 'ZINVOICE'
#   IMPORTING
#     number      = lv_invoice_nr.

# Output Option 1: UUID4 (globally unique, no coordination needed)
import uuid
from dataclasses import dataclass
from datetime import datetime

@dataclass
class Invoice:
    id: str
    invoice_number: str
    created_at: datetime

def create_invoice() -> Invoice:
    """Create invoice with UUID-based ID"""
    invoice_id = str(uuid.uuid4())
    # Format: INV-{timestamp}-{short-uuid}
    invoice_number = f"INV-{datetime.now().strftime('%Y%m%d')}-{uuid.uuid4().hex[:8].upper()}"

    return Invoice(
        id=invoice_id,
        invoice_number=invoice_number,
        created_at=datetime.now()
    )

# Output Option 2: Database Sequence (sequential, database-managed)
from sqlalchemy import create_engine, Sequence, Integer, Column
from sqlalchemy.orm import declarative_base

Base = declarative_base()

class Invoice(Base):
    __tablename__ = 'invoices'

    # Auto-incrementing primary key
    id = Column(Integer, primary_key=True, autoincrement=True)

    # Custom sequence for invoice number
    invoice_number = Column(
        Integer,
        Sequence('invoice_nr_seq', start=1000, increment=1),
        unique=True
    )

# Output Option 3: Redis-based distributed ID (high-volume, distributed)
import redis
from datetime import datetime

class DistributedIDGenerator:
    """Distributed ID generator using Redis"""

    def __init__(self, redis_client: redis.Redis, prefix: str = "INV"):
        self.redis = redis_client
        self.prefix = prefix

    def get_next_id(self, range_name: str = "default") -> str:
        """Get next ID from Redis counter

        Args:
            range_name: Name of the number range

        Returns:
            Formatted ID string
        """
        # Atomic increment in Redis
        counter = self.redis.incr(f"nr_range:{range_name}")

        # Format: PREFIX-YYYYMMDD-000001
        date_str = datetime.now().strftime('%Y%m%d')
        return f"{self.prefix}-{date_str}-{counter:06d}"

# Usage
redis_client = redis.Redis(host='localhost', port=6379)
id_gen = DistributedIDGenerator(redis_client, prefix="INV")
invoice_number = id_gen.get_next_id("invoices")
```

#### Pattern 8: ABAP Authorization → Python RBAC

```python
# Input: ABAP authorization check
# AUTHORITY-CHECK OBJECT 'Z_FLIGHT'
#   ID 'CARRID' FIELD lv_carrid
#   ID 'ACTVT'  FIELD '03'.  "Display
# IF sy-subrc <> 0.
#   RAISE EXCEPTION TYPE zcx_no_authorization.
# ENDIF.

# Output Option 1: Decorator-based authorization (simple)
from functools import wraps
from typing import Callable, List
from fastapi import HTTPException, Depends
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials

class UnauthorizedError(Exception):
    """Authorization error"""
    pass

def require_permission(permission: str):
    """Decorator to check user permissions

    Args:
        permission: Required permission (e.g., 'flight:read', 'flight:write')
    """
    def decorator(func: Callable):
        @wraps(func)
        async def wrapper(*args, **kwargs):
            # Get current user from context (FastAPI dependency)
            user = kwargs.get('current_user')

            if not user or permission not in user.permissions:
                raise HTTPException(
                    status_code=403,
                    detail=f"Permission denied: {permission} required"
                )

            return await func(*args, **kwargs)
        return wrapper
    return decorator

# Usage with FastAPI
@app.get("/flights/{carrid}")
@require_permission("flight:read")
async def get_flight(carrid: str, current_user: User = Depends(get_current_user)):
    """Get flight data (requires flight:read permission)"""
    return await flight_service.get_by_carrier(carrid)

# Output Option 2: Casbin for complex authorization (ABAP-like rules)
import casbin
from pathlib import Path

class AuthorizationService:
    """RBAC authorization using Casbin"""

    def __init__(self, model_path: str, policy_path: str):
        self.enforcer = casbin.Enforcer(model_path, policy_path)

    def check_authorization(
        self,
        user: str,
        resource: str,
        action: str,
        **fields
    ) -> bool:
        """Check if user is authorized

        Args:
            user: User ID
            resource: Resource name (e.g., 'flight')
            action: Action (e.g., 'read', 'write', 'delete')
            **fields: Additional authorization fields

        Returns:
            True if authorized

        Raises:
            UnauthorizedError: If not authorized
        """
        # Check base permission
        if not self.enforcer.enforce(user, resource, action):
            raise UnauthorizedError(
                f"User {user} not authorized for {action} on {resource}"
            )

        # Check field-level authorization (like ABAP ID fields)
        for field_name, field_value in fields.items():
            if not self.enforcer.enforce(user, resource, field_name, field_value):
                raise UnauthorizedError(
                    f"User {user} not authorized for {field_name}={field_value}"
                )

        return True

# model.conf
# [request_definition]
# r = sub, obj, act
#
# [policy_definition]
# p = sub, obj, act
#
# [role_definition]
# g = _, _
#
# [policy_effect]
# e = some(where (p.eft == allow))
#
# [matchers]
# m = g(r.sub, p.sub) && r.obj == p.obj && r.act == p.act

# policy.csv
# p, admin, flight, read
# p, admin, flight, write
# p, user, flight, read
# g, alice, admin
# g, bob, user
```

#### Pattern 9: ABAP Background Jobs → Python Celery

```python
# Input: ABAP background job
# CALL FUNCTION 'JOB_OPEN'
#   EXPORTING
#     jobname  = 'ZINVOICE_PROCESS'
#   IMPORTING
#     jobcount = lv_jobcount.
#
# SUBMIT zinvoice_processor
#   VIA JOB 'ZINVOICE_PROCESS' NUMBER lv_jobcount
#   AND RETURN.
#
# CALL FUNCTION 'JOB_CLOSE'
#   EXPORTING
#     jobcount = lv_jobcount
#     strtimmed = 'X'.

# Output: Celery task (recommended for async processing)
from celery import Celery, Task
from celery.schedules import crontab
from datetime import datetime
import logging

# Initialize Celery
app = Celery('invoice_processor')
app.config_from_object('celeryconfig')

# Configure periodic tasks
app.conf.beat_schedule = {
    'process-invoices-every-hour': {
        'task': 'tasks.process_invoices',
        'schedule': crontab(minute=0),  # Every hour
    },
    'cleanup-old-data-daily': {
        'task': 'tasks.cleanup_old_data',
        'schedule': crontab(hour=2, minute=0),  # 2 AM daily
    },
}

@app.task(bind=True, max_retries=3)
def process_invoices(self: Task) -> dict:
    """Process pending invoices (background job)

    Returns:
        dict: Processing result with counts
    """
    try:
        logger = logging.getLogger(__name__)
        logger.info("Starting invoice processing job")

        # Fetch pending invoices
        pending = Invoice.query.filter_by(status='pending').all()

        processed = 0
        errors = 0

        for invoice in pending:
            try:
                # Process invoice
                result = invoice.process()
                processed += 1
            except Exception as e:
                logger.error(f"Error processing invoice {invoice.id}: {e}")
                errors += 1

        logger.info(f"Job complete: {processed} processed, {errors} errors")

        return {
            'total': len(pending),
            'processed': processed,
            'errors': errors,
            'timestamp': datetime.now().isoformat()
        }

    except Exception as exc:
        # Retry with exponential backoff
        raise self.retry(exc=exc, countdown=60 * (2 ** self.request.retries))

# Manual job submission (equivalent to SM36)
from celery import group, chain, chord

# Submit single task
result = process_invoices.delay()

# Submit task with specific time
from datetime import timedelta
process_invoices.apply_async(eta=datetime.now() + timedelta(hours=1))

# Job chain (sequential dependencies)
workflow = chain(
    process_invoices.s(),
    send_notifications.s(),
    cleanup_temp_data.s()
)
workflow.apply_async()

# Parallel processing (job group)
job = group(
    process_invoices.s(),
    process_orders.s(),
    process_shipments.s()
)
result = job.apply_async()

# celeryconfig.py
broker_url = 'redis://localhost:6379/0'
result_backend = 'redis://localhost:6379/1'
task_serializer = 'json'
result_serializer = 'json'
accept_content = ['json']
timezone = 'UTC'
enable_utc = True
task_track_started = True
task_time_limit = 30 * 60  # 30 minutes
worker_prefetch_multiplier = 1
```

#### Pattern 10: ABAP Lock Objects → Python Redis Locks

```python
# Input: ABAP enqueue/dequeue
# CALL FUNCTION 'ENQUEUE_EZFLIGHT'
#   EXPORTING
#     mode_zdemo_abap_fli = 'E'  "Exclusive
#     carrid              = lv_carrid
#     connid              = lv_connid
#   EXCEPTIONS
#     foreign_lock        = 1
#     system_failure      = 2.
#
# " ... critical section ...
#
# CALL FUNCTION 'DEQUEUE_EZFLIGHT'
#   EXPORTING
#     carrid = lv_carrid
#     connid = lv_connid.

# Output Option 1: Redis distributed lock (for multi-server deployments)
import redis
from contextlib import contextmanager
from typing import Optional
import time

class DistributedLock:
    """Distributed lock using Redis (equivalent to ABAP ENQUEUE)"""

    def __init__(self, redis_client: redis.Redis, lock_name: str, timeout: int = 10):
        """
        Args:
            redis_client: Redis connection
            lock_name: Unique lock identifier
            timeout: Lock timeout in seconds
        """
        self.redis = redis_client
        self.lock_name = f"lock:{lock_name}"
        self.timeout = timeout
        self.lock_value = None

    def acquire(self, blocking: bool = True, retry_interval: float = 0.1) -> bool:
        """Acquire lock (equivalent to ENQUEUE)

        Args:
            blocking: Wait for lock if True
            retry_interval: Time between retries in seconds

        Returns:
            True if lock acquired

        Raises:
            LockAcquireError: If lock cannot be acquired (equivalent to foreign_lock)
        """
        import uuid
        self.lock_value = str(uuid.uuid4())

        if blocking:
            start_time = time.time()
            while time.time() - start_time < self.timeout:
                # Try to set lock with NX (only if not exists) and EX (expiry)
                if self.redis.set(
                    self.lock_name,
                    self.lock_value,
                    nx=True,
                    ex=self.timeout
                ):
                    return True
                time.sleep(retry_interval)

            raise LockAcquireError(f"Could not acquire lock {self.lock_name}")
        else:
            # Non-blocking acquire
            return self.redis.set(
                self.lock_name,
                self.lock_value,
                nx=True,
                ex=self.timeout
            )

    def release(self):
        """Release lock (equivalent to DEQUEUE)"""
        # Use Lua script to ensure atomicity (only delete if we own the lock)
        lua_script = """
        if redis.call("get", KEYS[1]) == ARGV[1] then
            return redis.call("del", KEYS[1])
        else
            return 0
        end
        """
        self.redis.eval(lua_script, 1, self.lock_name, self.lock_value)
        self.lock_value = None

@contextmanager
def flight_lock(carrid: str, connid: str, redis_client: redis.Redis):
    """Context manager for flight lock (ABAP-style usage)

    Args:
        carrid: Carrier ID
        connid: Connection ID
        redis_client: Redis connection

    Yields:
        Lock object

    Example:
        with flight_lock('AA', '0001', redis_client):
            # Critical section - update flight data
            flight.seats_occupied += 1
            flight.save()
    """
    lock_name = f"flight:{carrid}:{connid}"
    lock = DistributedLock(redis_client, lock_name, timeout=30)

    try:
        lock.acquire(blocking=True)
        yield lock
    finally:
        lock.release()

# Usage
redis_client = redis.Redis(host='localhost', port=6379)

try:
    with flight_lock('AA', '0001', redis_client):
        # Update flight - protected by lock
        flight = Flight.query.filter_by(carrid='AA', connid='0001').first()
        flight.seats_occupied += 1
        db.session.commit()
except LockAcquireError as e:
    print(f"Flight is locked by another process: {e}")

# Output Option 2: SQLAlchemy pessimistic locking (single server)
from sqlalchemy.orm import Session

with Session() as session:
    # SELECT FOR UPDATE (database-level lock)
    flight = session.query(Flight)\
        .filter_by(carrid='AA', connid='0001')\
        .with_for_update()\
        .first()

    # Critical section
    flight.seats_occupied += 1
    session.commit()
    # Lock released automatically on commit
```

#### Pattern 11: ABAP Message Classes → Python i18n

```python
# Input: ABAP message class
# MESSAGE e001(zinvoice) WITH lv_invoice_nr INTO lv_msg.
# " Message: Invoice &1 not found

# Output: Python i18n with gettext/babel
import gettext
from pathlib import Path
from typing import Optional
from enum import Enum

class MessageType(Enum):
    """Message types (ABAP: E/W/I/S/A/X)"""
    ERROR = "E"
    WARNING = "W"
    INFO = "I"
    SUCCESS = "S"
    ABORT = "A"

class MessageClass:
    """Message class handler (equivalent to ABAP message class)"""

    def __init__(self, message_class: str, locale: str = 'en'):
        """
        Args:
            message_class: Message class name (e.g., 'invoice')
            locale: Language locale (e.g., 'en', 'de', 'fr')
        """
        # Load translations
        locale_path = Path(__file__).parent / 'locales'
        self.translation = gettext.translation(
            message_class,
            localedir=locale_path,
            languages=[locale],
            fallback=True
        )
        self._ = self.translation.gettext

    def get_message(
        self,
        msg_type: MessageType,
        msg_number: str,
        *args
    ) -> str:
        """Get formatted message

        Args:
            msg_type: Message type (E/W/I/S)
            msg_number: Message number (e.g., '001')
            *args: Message parameters

        Returns:
            Formatted message string
        """
        # Message key: {type}{number}
        msg_key = f"{msg_type.value}{msg_number}"

        # Get translated message template
        template = self._(msg_key)

        # Format with parameters (&1, &2, &3 → {0}, {1}, {2})
        return template.format(*args)

# messages.pot (message catalog template)
# msgid "E001"
# msgstr "Invoice {0} not found"
#
# msgid "E002"
# msgstr "Invoice {0} already processed"
#
# msgid "W001"
# msgstr "Invoice {0} has no line items"
#
# msgid "I001"
# msgstr "Invoice {0} created successfully"

# Usage
messages = MessageClass('invoice', locale='en')

# E001: Invoice INV-001 not found
error_msg = messages.get_message(MessageType.ERROR, '001', 'INV-001')

# With FastAPI exception handling
from fastapi import HTTPException

class BusinessError(HTTPException):
    """Business error with message class"""

    def __init__(
        self,
        message_class: str,
        msg_type: MessageType,
        msg_number: str,
        *args,
        locale: str = 'en'
    ):
        messages = MessageClass(message_class, locale)
        detail = messages.get_message(msg_type, msg_number, *args)

        status_code = {
            MessageType.ERROR: 400,
            MessageType.WARNING: 400,
            MessageType.INFO: 200,
        }.get(msg_type, 500)

        super().__init__(status_code=status_code, detail=detail)

# Usage in API
if not invoice:
    raise BusinessError('invoice', MessageType.ERROR, '001', invoice_nr)
```

#### Pattern 12: ABAP Selection Screens → Python Query Models

```python
# Input: ABAP selection screen
# PARAMETERS: p_carrid TYPE s_carr_id.
# SELECT-OPTIONS: s_connid FOR ls_flight-connid.
# SELECT-OPTIONS: s_date FOR ls_flight-fldate.
# PARAMETERS: p_maxrow TYPE i DEFAULT 100.

# Output: Pydantic query model (FastAPI)
from pydantic import BaseModel, Field, validator
from typing import Optional, List
from datetime import date
from enum import Enum

class DateRange(BaseModel):
    """Date range filter (SELECT-OPTIONS equivalent)"""
    from_date: Optional[date] = Field(None, description="Start date (inclusive)")
    to_date: Optional[date] = Field(None, description="End date (inclusive)")

    @validator('to_date')
    def validate_date_range(cls, v, values):
        if v and 'from_date' in values and values['from_date']:
            if v < values['from_date']:
                raise ValueError('to_date must be >= from_date')
        return v

class ValueRange(BaseModel):
    """Generic range filter for SELECT-OPTIONS"""
    low: Optional[str] = None
    high: Optional[str] = None
    option: str = Field('BT', description="Option: EQ, BT, NE, etc.")

    # EQ: Equal, BT: Between, NE: Not equal, GT: Greater than, LT: Less than

class FlightQuery(BaseModel):
    """Flight search parameters (selection screen equivalent)"""

    # PARAMETERS equivalent
    carrid: Optional[str] = Field(
        None,
        max_length=3,
        description="Carrier ID"
    )

    # SELECT-OPTIONS equivalent (simple range)
    connid_from: Optional[str] = Field(None, description="Connection ID from")
    connid_to: Optional[str] = Field(None, description="Connection ID to")

    # SELECT-OPTIONS with multiple ranges
    connid_in: Optional[List[str]] = Field(
        None,
        description="List of connection IDs"
    )

    # Date range
    flight_date: Optional[DateRange] = Field(
        None,
        description="Flight date range"
    )

    # PARAMETERS with default
    max_rows: int = Field(
        100,
        ge=1,
        le=1000,
        description="Maximum rows to return"
    )

    # Additional filters
    class_: Optional[str] = Field(None, alias='class', description="Flight class")

    @validator('carrid')
    def validate_carrid(cls, v):
        if v and not v.isalnum():
            raise ValueError('Carrier ID must be alphanumeric')
        return v.upper() if v else v

# FastAPI endpoint with query model
from fastapi import APIRouter, Query, Depends
from sqlalchemy.orm import Session

router = APIRouter()

@router.get("/flights/search")
async def search_flights(
    query: FlightQuery = Depends(),
    db: Session = Depends(get_db)
):
    """Search flights with selection screen-like parameters"""

    # Build query
    stmt = select(Flight)

    # Apply filters
    if query.carrid:
        stmt = stmt.where(Flight.carrid == query.carrid)

    if query.connid_from and query.connid_to:
        stmt = stmt.where(Flight.connid.between(query.connid_from, query.connid_to))

    if query.connid_in:
        stmt = stmt.where(Flight.connid.in_(query.connid_in))

    if query.flight_date:
        if query.flight_date.from_date:
            stmt = stmt.where(Flight.fldate >= query.flight_date.from_date)
        if query.flight_date.to_date:
            stmt = stmt.where(Flight.fldate <= query.flight_date.to_date)

    # Apply limit
    stmt = stmt.limit(query.max_rows)

    # Execute
    results = db.execute(stmt).scalars().all()

    return {
        'count': len(results),
        'max_rows': query.max_rows,
        'data': results
    }
```

#### Pattern 13: ABAP ALV Grids → Python Data Tables

```python
# Input: ABAP ALV grid display
# DATA: lt_flights TYPE TABLE OF zdemo_abap_fli.
#
# CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
#   EXPORTING
#     i_callback_program = sy-repid
#     it_fieldcat        = lt_fieldcat
#   TABLES
#     t_outtab           = lt_flights.

# Output: FastAPI with pagination and filtering
from fastapi import APIRouter, Query
from pydantic import BaseModel
from typing import List, Optional, Generic, TypeVar
from sqlalchemy import select, func
from sqlalchemy.orm import Session

T = TypeVar('T')

class PageParams(BaseModel):
    """Pagination parameters"""
    page: int = Field(1, ge=1, description="Page number")
    page_size: int = Field(50, ge=1, le=1000, description="Items per page")
    sort_by: Optional[str] = Field(None, description="Sort field")
    sort_order: str = Field('asc', regex='^(asc|desc)$', description="Sort order")

class PagedResponse(BaseModel, Generic[T]):
    """Paginated response (ALV grid equivalent)"""
    total: int = Field(..., description="Total number of records")
    page: int = Field(..., description="Current page")
    page_size: int = Field(..., description="Items per page")
    total_pages: int = Field(..., description="Total pages")
    data: List[T] = Field(..., description="Page data")

    class Config:
        from_attributes = True

class FlightFilter(BaseModel):
    """Flight filters (ALV filter equivalent)"""
    carrid: Optional[str] = None
    connid: Optional[str] = None
    min_price: Optional[float] = None
    max_price: Optional[float] = None
    search: Optional[str] = Field(None, description="Free text search")

class FlightResponse(BaseModel):
    """Flight data (ALV column configuration)"""
    carrid: str
    connid: str
    flight_date: date
    price: float
    currency: str
    seats_max: int
    seats_occupied: int

    @property
    def seats_available(self) -> int:
        """Calculated field (like ALV formula columns)"""
        return self.seats_max - self.seats_occupied

@router.get("/flights/grid", response_model=PagedResponse[FlightResponse])
async def get_flights_grid(
    page_params: PageParams = Depends(),
    filters: FlightFilter = Depends(),
    db: Session = Depends(get_db)
):
    """Get flights in paginated grid format (ALV equivalent)

    Features:
    - Pagination (page, page_size)
    - Sorting (sort_by, sort_order)
    - Filtering (carrid, connid, price range)
    - Search (free text)
    - Calculated fields (seats_available)
    """

    # Base query
    query = select(Flight)

    # Apply filters
    if filters.carrid:
        query = query.where(Flight.carrid == filters.carrid)
    if filters.connid:
        query = query.where(Flight.connid == filters.connid)
    if filters.min_price:
        query = query.where(Flight.price >= filters.min_price)
    if filters.max_price:
        query = query.where(Flight.price <= filters.max_price)
    if filters.search:
        search_pattern = f"%{filters.search}%"
        query = query.where(
            (Flight.carrid.like(search_pattern)) |
            (Flight.connid.like(search_pattern))
        )

    # Get total count
    count_query = select(func.count()).select_from(query.subquery())
    total = db.execute(count_query).scalar()

    # Apply sorting
    if page_params.sort_by:
        sort_column = getattr(Flight, page_params.sort_by, None)
        if sort_column:
            if page_params.sort_order == 'desc':
                query = query.order_by(sort_column.desc())
            else:
                query = query.order_by(sort_column.asc())

    # Apply pagination
    offset = (page_params.page - 1) * page_params.page_size
    query = query.offset(offset).limit(page_params.page_size)

    # Execute
    results = db.execute(query).scalars().all()

    # Calculate total pages
    total_pages = (total + page_params.page_size - 1) // page_params.page_size

    return PagedResponse(
        total=total,
        page=page_params.page,
        page_size=page_params.page_size,
        total_pages=total_pages,
        data=results
    )

# Optional: CSV export (ALV export equivalent)
import csv
from fastapi.responses import StreamingResponse
import io

@router.get("/flights/export/csv")
async def export_flights_csv(
    filters: FlightFilter = Depends(),
    db: Session = Depends(get_db)
):
    """Export flights to CSV (ALV export equivalent)"""

    # Get all matching records (no pagination)
    query = select(Flight)
    # ... apply filters ...
    results = db.execute(query).scalars().all()

    # Create CSV
    output = io.StringIO()
    writer = csv.DictWriter(
        output,
        fieldnames=['carrid', 'connid', 'flight_date', 'price', 'currency']
    )
    writer.writeheader()

    for flight in results:
        writer.writerow({
            'carrid': flight.carrid,
            'connid': flight.connid,
            'flight_date': flight.flight_date.isoformat(),
            'price': float(flight.price),
            'currency': flight.currency
        })

    output.seek(0)

    return StreamingResponse(
        iter([output.getvalue()]),
        media_type="text/csv",
        headers={"Content-Disposition": "attachment; filename=flights.csv"}
    )
```

#### Pattern 14: ABAP Synchronous Code → Python Async/Await

```python
# Input: Synchronous ABAP code
# SELECT * FROM zdemo_abap_fli INTO TABLE @DATA(lt_flights).
# LOOP AT lt_flights INTO DATA(ls_flight).
#   CALL FUNCTION 'Z_GET_FLIGHT_DETAILS'
#     EXPORTING iv_carrid = ls_flight-carrid
#     IMPORTING es_details = ls_details.
# ENDLOOP.

# Output: Async Python (FastAPI recommended pattern)
import asyncio
from typing import List, Optional
from sqlalchemy.ext.asyncio import AsyncSession, create_async_engine
from sqlalchemy import select
import httpx
from fastapi import APIRouter, Depends

from app.dependencies import get_async_db  # adjust to your project structure

router = APIRouter()

# Async database operations
async def get_flights(db: AsyncSession, carrid: Optional[str] = None) -> List[Flight]:
    """Async database query"""
    query = select(Flight)
    if carrid:
        query = query.where(Flight.carrid == carrid)

    result = await db.execute(query)
    return result.scalars().all()

async def get_flight_details(carrid: str, connid: str) -> dict:
    """Async external service call"""
    async with httpx.AsyncClient() as client:
        response = await client.get(
            f"https://api.example.com/flights/{carrid}/{connid}"
        )
        return response.json()

# Parallel processing with asyncio.gather
async def enrich_flights_parallel(flights: List[Flight]) -> List[dict]:
    """Process flights in parallel (much faster than LOOP)"""

    # Create tasks for all flights
    tasks = [
        get_flight_details(flight.carrid, flight.connid)
        for flight in flights
    ]

    # Execute all tasks in parallel
    details_list = await asyncio.gather(*tasks)

    # Combine flight data with details
    enriched = []
    for flight, details in zip(flights, details_list):
        enriched.append({
            **flight.__dict__,
            'details': details
        })

    return enriched

# FastAPI endpoint
@router.get("/flights/enriched")
async def get_enriched_flights(
    carrid: Optional[str] = None,
    db: AsyncSession = Depends(get_async_db)
):
    """Get flights with enriched data (async for performance)"""

    # Async database query
    flights = await get_flights(db, carrid)

    # Parallel enrichment
    enriched = await enrich_flights_parallel(flights)

    return {'count': len(enriched), 'data': enriched}

# Background tasks (async version of ABAP background jobs)
from fastapi import BackgroundTasks

async def process_flight_async(flight_id: str):
    """Async background processing"""
    await asyncio.sleep(5)  # Simulate processing
    # Update database, send notifications, etc.

@router.post("/flights/{flight_id}/process")
async def trigger_processing(
    flight_id: str,
    background_tasks: BackgroundTasks
):
    """Trigger async background processing"""
    background_tasks.add_task(process_flight_async, flight_id)
    return {'status': 'processing', 'flight_id': flight_id}
```

#### Pattern 15: ABAP Events → Python Event-Driven Architecture

```python
# Input: ABAP event handling
# CLASS lcl_handler DEFINITION.
#   PUBLIC SECTION.
#     METHODS on_data_changed
#       FOR EVENT data_changed OF cl_gui_alv_grid
#       IMPORTING er_data_changed.
# ENDCLASS.

# Output: Event-driven architecture with event bus
from dataclasses import dataclass
from typing import Callable, Dict, List, Any
from datetime import datetime
from enum import Enum

class EventType(Enum):
    """Event types"""
    FLIGHT_CREATED = "flight.created"
    FLIGHT_UPDATED = "flight.updated"
    FLIGHT_DELETED = "flight.deleted"
    BOOKING_CREATED = "booking.created"
    PAYMENT_RECEIVED = "payment.received"

@dataclass
class Event:
    """Event data structure"""
    event_type: EventType
    data: Dict[str, Any]
    timestamp: datetime
    correlation_id: str = None

class EventBus:
    """In-memory event bus (for single server)"""

    def __init__(self):
        self._handlers: Dict[EventType, List[Callable]] = {}

    def subscribe(self, event_type: EventType, handler: Callable):
        """Subscribe to event

        Args:
            event_type: Type of event to subscribe to
            handler: Async function to handle event
        """
        if event_type not in self._handlers:
            self._handlers[event_type] = []
        self._handlers[event_type].append(handler)

    async def publish(self, event: Event):
        """Publish event to all subscribers

        Args:
            event: Event to publish
        """
        handlers = self._handlers.get(event.event_type, [])

        # Execute all handlers in parallel
        await asyncio.gather(*[
            handler(event) for handler in handlers
        ])

# Global event bus
event_bus = EventBus()

# Event handlers (subscribers)
async def on_flight_created(event: Event):
    """Handle flight created event"""
    print(f"Flight created: {event.data}")
    # Send notification, update cache, etc.

async def on_booking_created(event: Event):
    """Handle booking created event"""
    # Update inventory, send confirmation email
    booking_id = event.data['booking_id']
    await send_confirmation_email(booking_id)

# Register handlers
event_bus.subscribe(EventType.FLIGHT_CREATED, on_flight_created)
event_bus.subscribe(EventType.BOOKING_CREATED, on_booking_created)

# Publish events (from business logic)
async def create_flight(flight_data: dict):
    """Create flight and publish event"""

    # Create flight
    flight = Flight(**flight_data)
    db.add(flight)
    db.commit()

    # Publish event
    event = Event(
        event_type=EventType.FLIGHT_CREATED,
        data={'flight_id': flight.id, 'carrid': flight.carrid},
        timestamp=datetime.now()
    )
    await event_bus.publish(event)

    return flight

# For distributed systems: RabbitMQ/Kafka integration
from aio_pika import connect, Message, ExchangeType
import json

class DistributedEventBus:
    """Distributed event bus using RabbitMQ"""

    def __init__(self, amqp_url: str):
        self.amqp_url = amqp_url
        self.connection = None
        self.channel = None
        self.exchange = None

    async def connect(self):
        """Connect to RabbitMQ"""
        self.connection = await connect(self.amqp_url)
        self.channel = await self.connection.channel()
        self.exchange = await self.channel.declare_exchange(
            'events',
            ExchangeType.TOPIC
        )

    async def publish(self, event: Event):
        """Publish event to RabbitMQ"""
        message_body = json.dumps({
            'event_type': event.event_type.value,
            'data': event.data,
            'timestamp': event.timestamp.isoformat()
        })

        message = Message(
            body=message_body.encode(),
            content_type='application/json'
        )

        await self.exchange.publish(
            message,
            routing_key=event.event_type.value
        )

    async def subscribe(self, event_type: EventType, handler: Callable):
        """Subscribe to events from RabbitMQ"""
        queue = await self.channel.declare_queue(
            f'queue.{event_type.value}',
            durable=True
        )

        await queue.bind(self.exchange, routing_key=event_type.value)

        async def on_message(message):
            async with message.process():
                data = json.loads(message.body.decode())
                event = Event(
                    event_type=EventType(data['event_type']),
                    data=data['data'],
                    timestamp=datetime.fromisoformat(data['timestamp'])
                )
                await handler(event)

        await queue.consume(on_message)
```

## Framework-Specific Implementations

### FastAPI (Recommended for REST APIs)

```python
from fastapi import APIRouter, Depends, HTTPException, status
from sqlalchemy.orm import Session
from pydantic import BaseModel, Field

# Request/Response models
class FlightCreate(BaseModel):
    carrid: str = Field(..., max_length=3)
    connid: str = Field(..., max_length=4)
    fldate: date
    price: Decimal

class FlightResponse(BaseModel):
    id: int
    carrid: str
    connid: str
    fldate: date
    price: Decimal

    class Config:
        from_attributes = True

# Router
router = APIRouter(prefix="/api/v1/flights", tags=["flights"])

@router.post("/", response_model=FlightResponse, status_code=status.HTTP_201_CREATED)
async def create_flight(
    flight: FlightCreate,
    db: Session = Depends(get_db)
):
    """Create a new flight (equivalent to ABAP RAP CREATE)"""
    try:
        db_flight = Flight(**flight.dict())
        db.add(db_flight)
        db.commit()
        db.refresh(db_flight)
        return db_flight
    except Exception as e:
        db.rollback()
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=str(e)
        )

@router.get("/{carrid}/{connid}", response_model=FlightResponse)
async def get_flight(
    carrid: str,
    connid: str,
    db: Session = Depends(get_db)
):
    """Get flight by key (equivalent to ABAP RAP READ)"""
    flight = db.query(Flight).filter(
        Flight.carrid == carrid,
        Flight.connid == connid
    ).first()

    if not flight:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="Flight not found"
        )

    return flight
```

### Django (Recommended for Full Applications)

```python
from django.db import models
from rest_framework import viewsets, serializers
from rest_framework.decorators import action
from rest_framework.response import Response

# Models
class Flight(models.Model):
    carrid = models.CharField(max_length=3)
    connid = models.CharField(max_length=4)
    fldate = models.DateField()
    price = models.DecimalField(max_digits=10, decimal_places=2)

    class Meta:
        db_table = 'flights'
        unique_together = [['carrid', 'connid', 'fldate']]

# Serializers
class FlightSerializer(serializers.ModelSerializer):
    class Meta:
        model = Flight
        fields = '__all__'

# ViewSet
class FlightViewSet(viewsets.ModelViewSet):
    queryset = Flight.objects.all()
    serializer_class = FlightSerializer

    @action(detail=False, methods=['get'])
    def by_carrier(self, request):
        """Get flights by carrier (custom action)"""
        carrid = request.query_params.get('carrid')
        flights = self.queryset.filter(carrid=carrid)
        serializer = self.get_serializer(flights, many=True)
        return Response(serializer.data)
```

## Deployment and Infrastructure

### Docker Configuration

**Dockerfile for FastAPI:**
```dockerfile
FROM python:3.11-slim

WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y \
    gcc \
    postgresql-client \
    && rm -rf /var/lib/apt/lists/*

# Copy requirements
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Copy application code
COPY ./app /app

# Create non-root user
RUN useradd -m -u 1000 appuser && chown -R appuser:appuser /app
USER appuser

EXPOSE 8000

# Run with uvicorn
CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8000"]
```

**docker-compose.yml:**
```yaml
version: '3.8'

services:
  api:
    build: .
    ports:
      - "8000:8000"
    environment:
      - DATABASE_URL=postgresql://user:password@db:5432/appdb
      - REDIS_URL=redis://redis:6379/0
    depends_on:
      - db
      - redis
    volumes:
      - ./app:/app

  db:
    image: postgres:15-alpine
    environment:
      - POSTGRES_USER=user
      - POSTGRES_PASSWORD=password
      - POSTGRES_DB=appdb
    volumes:
      - postgres_data:/var/lib/postgresql/data
    ports:
      - "5432:5432"

  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"
    volumes:
      - redis_data:/data

  celery_worker:
    build: .
    command: celery -A app.celery_app worker --loglevel=info
    environment:
      - DATABASE_URL=postgresql://user:password@db:5432/appdb
      - REDIS_URL=redis://redis:6379/0
    depends_on:
      - db
      - redis

  celery_beat:
    build: .
    command: celery -A app.celery_app beat --loglevel=info
    environment:
      - REDIS_URL=redis://redis:6379/0
    depends_on:
      - redis

volumes:
  postgres_data:
  redis_data:
```

### Environment Management

**`.env.example`:**
```bash
# Database
DATABASE_URL=postgresql://user:password@localhost:5432/appdb

# Redis
REDIS_URL=redis://localhost:6379/0

# Application
APP_NAME=ABAP Migration App
DEBUG=False
LOG_LEVEL=INFO

# Security
SECRET_KEY=your-secret-key-here
ACCESS_TOKEN_EXPIRE_MINUTES=30

# CORS
ALLOWED_ORIGINS=http://localhost:3000,https://app.example.com
```

**Configuration Management:**
```python
from pydantic import BaseSettings
from typing import List

class Settings(BaseSettings):
    """Application settings"""

    # Database
    database_url: str

    # Redis
    redis_url: str

    # Application
    app_name: str = "ABAP Migration App"
    debug: bool = False
    log_level: str = "INFO"

    # Security
    secret_key: str
    access_token_expire_minutes: int = 30

    # CORS
    allowed_origins: List[str] = []

    class Config:
        env_file = ".env"
        case_sensitive = False

# Global settings instance
settings = Settings()
```

### Database Migrations (Alembic)

**alembic.ini:**
```ini
[alembic]
script_location = alembic
sqlalchemy.url = postgresql://user:password@localhost/appdb

[loggers]
keys = root,sqlalchemy,alembic

[handlers]
keys = console

[formatters]
keys = generic
```

**Migration Commands:**
```bash
# Initialize Alembic
alembic init alembic

# Create migration
alembic revision --autogenerate -m "Create flight table"

# Run migrations
alembic upgrade head

# Rollback migration
alembic downgrade -1
```

**Example Migration:**
```python
"""Create flight table

Revision ID: 001
"""
from alembic import op
import sqlalchemy as sa
from sqlalchemy.dialects import postgresql

def upgrade():
    op.create_table(
        'flights',
        sa.Column('id', sa.Integer(), primary_key=True),
        sa.Column('carrid', sa.String(3), nullable=False),
        sa.Column('connid', sa.String(4), nullable=False),
        sa.Column('fldate', sa.Date(), nullable=False),
        sa.Column('price', sa.Numeric(10, 2), nullable=False),
        sa.Column('currency', sa.String(3), nullable=False),
        sa.Column('created_at', sa.DateTime(), server_default=sa.func.now()),
        sa.Column('updated_at', sa.DateTime(), onupdate=sa.func.now())
    )

    op.create_index('idx_flights_carrid', 'flights', ['carrid'])

def downgrade():
    op.drop_index('idx_flights_carrid')
    op.drop_table('flights')
```

### CI/CD Configuration

**GitHub Actions (.github/workflows/ci.yml):**
```yaml
name: CI/CD

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  test:
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

    steps:
    - uses: actions/checkout@v3

    - name: Set up Python
      uses: actions/setup-python@v4
      with:
        python-version: '3.11'

    - name: Cache dependencies
      uses: actions/cache@v3
      with:
        path: ~/.cache/pip
        key: ${{ runner.os }}-pip-${{ hashFiles('**/requirements.txt') }}

    - name: Install dependencies
      run: |
        pip install -r requirements.txt
        pip install pytest pytest-cov

    - name: Run tests
      env:
        DATABASE_URL: postgresql://test:test@localhost:5432/testdb
        REDIS_URL: redis://localhost:6379/0
      run: |
        pytest --cov=app --cov-report=xml

    - name: Upload coverage
      uses: codecov/codecov-action@v3

  build:
    needs: test
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'

    steps:
    - uses: actions/checkout@v3

    - name: Build and push Docker image
      uses: docker/build-push-action@v4
      with:
        context: .
        push: true
        tags: myregistry/myapp:latest
```

### Logging Configuration

```python
import logging
from logging.handlers import RotatingFileHandler
import sys

def setup_logging(log_level: str = "INFO"):
    """Configure application logging"""

    # Create logger
    logger = logging.getLogger("app")
    logger.setLevel(getattr(logging, log_level.upper()))

    # Console handler
    console_handler = logging.StreamHandler(sys.stdout)
    console_handler.setLevel(logging.DEBUG)

    # File handler with rotation
    file_handler = RotatingFileHandler(
        "logs/app.log",
        maxBytes=10485760,  # 10MB
        backupCount=10
    )
    file_handler.setLevel(logging.INFO)

    # Formatter
    formatter = logging.Formatter(
        '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    console_handler.setFormatter(formatter)
    file_handler.setFormatter(formatter)

    # Add handlers
    logger.addHandler(console_handler)
    logger.addHandler(file_handler)

    return logger
```

### Health Checks

```python
from fastapi import APIRouter, Depends
from sqlalchemy import text
from sqlalchemy.orm import Session
import logging
import redis

from app.dependencies import get_db, get_redis  # adjust import to your project structure

router = APIRouter()

@router.get("/health")
async def health_check():
    """Health check endpoint"""
    return {"status": "healthy"}

@router.get("/health/ready")
async def readiness_check(
    db: Session = Depends(get_db),
    redis_client: redis.Redis = Depends(get_redis)
):
    """Readiness check with dependencies"""
    checks = {
        "database": False,
        "redis": False
    }

    # Check database
    try:
        db.execute(text("SELECT 1"))
        checks["database"] = True
    except Exception as e:
        logging.error(f"Database check failed: {e}")

    # Check Redis
    try:
        redis_client.ping()
        checks["redis"] = True
    except Exception as e:
        logging.error(f"Redis check failed: {e}")

    # Overall status
    all_healthy = all(checks.values())

    return {
        "status": "ready" if all_healthy else "not ready",
        "checks": checks
    }
```

## Testing with pytest

```python
import pytest
from datetime import date
from decimal import Decimal

# Fixtures
@pytest.fixture
def sample_flight():
    """Create a sample flight for testing"""
    return Flight(
        carrid='AA',
        connid='0001',
        fldate=date(2024, 1, 15),
        price=Decimal('450.00')
    )

@pytest.fixture
def db_session():
    """Create test database session"""
    from sqlalchemy import create_engine
    from sqlalchemy.orm import sessionmaker

    engine = create_engine('sqlite:///:memory:')
    Base.metadata.create_all(engine)
    Session = sessionmaker(bind=engine)
    session = Session()

    yield session

    session.close()

# Test cases
def test_create_flight(db_session, sample_flight):
    """Test flight creation"""
    db_session.add(sample_flight)
    db_session.commit()

    result = db_session.query(Flight).filter_by(carrid='AA').first()
    assert result is not None
    assert result.connid == '0001'

def test_filter_flights():
    """Test filtering flights by criteria"""
    flights = [
        Flight('AA', '001', date(2024, 1, 1), Decimal('450')),
        Flight('LH', '002', date(2024, 1, 2), Decimal('520')),
        Flight('BA', '003', date(2024, 1, 3), Decimal('390')),
    ]

    expensive = [f for f in flights if f.price > Decimal('400')]
    assert len(expensive) == 2
```

## Output Format

When you translate ABAP code to Python, provide:

1. **Analysis Summary**: Brief description of what the ABAP code does
2. **Python Code**: Complete, runnable Python translation
3. **Dependencies**: Required packages (requirements.txt format)
4. **Tests**: pytest test cases
5. **Usage Example**: How to run/use the generated code
6. **Migration Notes**: Any important considerations or differences

## Example Output Structure

```markdown
## Analysis
The ABAP class zcl_flight_manager handles flight booking operations including
validation, pricing, and persistence.

## Python Translation

### Models (models.py)
[Generated dataclasses/ORM models]

### Business Logic (services.py)
[Generated service classes]

### API Endpoints (api.py)
[Generated FastAPI/Django endpoints]

### Tests (test_flights.py)
[Generated pytest tests]

## Dependencies
```
fastapi==0.104.1
sqlalchemy==2.0.23
pydantic==2.5.0
pytest==7.4.3
```

## Usage
```bash
pip install -r requirements.txt
uvicorn main:app --reload
```

## Migration Notes
- ABAP LUW transactions mapped to SQLAlchemy session transactions
- Authorization checks need to be implemented separately
- Consider adding API rate limiting for production
```

## Ready to Translate

Now, please provide the ABAP code you want to translate to Python, and specify:
1. Target framework (FastAPI, Django, or Flask)
2. Database (PostgreSQL, MySQL, SQLite)
3. Any specific requirements or constraints
