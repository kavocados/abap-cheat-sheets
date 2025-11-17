"""
ABAP Internal Tables to Python Collections Migration Example

This module demonstrates how to migrate common ABAP internal table operations
to equivalent Python code using modern Python idioms.

Based on: src/zcl_demo_abap_internal_tables.clas.abap
Reference: 01_Internal_Tables.md
"""

from dataclasses import dataclass, field
from typing import List, Optional, Dict, Any
from enum import Enum
from datetime import date
import itertools


# ==============================================================================
# Data Models (ABAP Types → Python dataclasses)
# ==============================================================================

@dataclass
class Flight:
    """
    Equivalent to ABAP structure for flight data
    ABAP: TYPES ty_flight TYPE zdemo_abap_fli
    """
    carrid: str  # Carrier ID
    connid: str  # Connection ID
    fldate: date  # Flight date
    price: float  # Price
    currency: str  # Currency
    seatsmax: int  # Maximum seats
    seatsocc: int  # Occupied seats


@dataclass
class Carrier:
    """
    Equivalent to ABAP carrier structure
    ABAP: TYPES ty_carrier TYPE zdemo_abap_carr
    """
    carrid: str  # Carrier ID
    carrname: str  # Carrier name
    currcode: str  # Currency code
    url: str = ""  # Website URL


# ==============================================================================
# Pattern 1: ABAP APPEND → Python list.append()
# ==============================================================================

def example_append_operation():
    """
    ABAP:
        DATA lt_flights TYPE TABLE OF ty_flight.
        APPEND VALUE #( carrid = 'AA' connid = '001' ) TO lt_flights.

    Python equivalent:
    """
    flights: List[Flight] = []

    # Simple append
    flights.append(Flight(
        carrid='AA',
        connid='001',
        fldate=date(2024, 1, 15),
        price=450.00,
        currency='USD',
        seatsmax=200,
        seatsocc=150
    ))

    return flights


# ==============================================================================
# Pattern 2: ABAP LOOP AT → Python for loop
# ==============================================================================

def example_loop_operations(flights: List[Flight]) -> List[Flight]:
    """
    ABAP:
        LOOP AT lt_flights INTO DATA(ls_flight).
          IF ls_flight-seatsmax > 100.
            ls_flight-price = ls_flight-price * '1.1'.
            MODIFY lt_flights FROM ls_flight.
          ENDIF.
        ENDLOOP.

    Python equivalent:
    """
    # Method 1: Direct modification (flights are mutable objects)
    for flight in flights:
        if flight.seatsmax > 100:
            flight.price = flight.price * 1.1

    # Method 2: Functional approach (immutable)
    updated_flights = [
        Flight(
            **{**flight.__dict__, 'price': flight.price * 1.1}
        ) if flight.seatsmax > 100 else flight
        for flight in flights
    ]

    return flights


# ==============================================================================
# Pattern 3: ABAP READ TABLE → Python filtering
# ==============================================================================

def example_read_table(flights: List[Flight], carrier: str) -> Optional[Flight]:
    """
    ABAP:
        READ TABLE lt_flights INTO DATA(ls_flight)
          WITH KEY carrid = 'AA'.

    Python equivalent:
    """
    # Method 1: Using next() with default
    flight = next(
        (f for f in flights if f.carrid == carrier),
        None
    )

    # Method 2: Using filter (if you need the first match)
    try:
        flight = next(filter(lambda f: f.carrid == carrier, flights))
    except StopIteration:
        flight = None

    return flight


# ==============================================================================
# Pattern 4: ABAP FILTER → Python list comprehension / filter()
# ==============================================================================

def example_filter_table(flights: List[Flight], min_seats: int = 150) -> List[Flight]:
    """
    ABAP:
        DATA(lt_filtered) = FILTER #( lt_flights
          WHERE seatsmax > 150 ).

    Python equivalent:
    """
    # Method 1: List comprehension (Pythonic)
    filtered = [f for f in flights if f.seatsmax > min_seats]

    # Method 2: filter() function
    filtered_alt = list(filter(lambda f: f.seatsmax > min_seats, flights))

    return filtered


# ==============================================================================
# Pattern 5: ABAP SORT → Python sorted() / list.sort()
# ==============================================================================

def example_sort_table(flights: List[Flight]) -> List[Flight]:
    """
    ABAP:
        SORT lt_flights BY carrid ASCENDING connid DESCENDING.

    Python equivalent:
    """
    # Method 1: In-place sort
    flights.sort(key=lambda f: (f.carrid, -float(f.connid)))

    # Method 2: Create new sorted list
    sorted_flights = sorted(
        flights,
        key=lambda f: (f.carrid, f.connid),
        reverse=False
    )

    # Method 3: Multiple sort keys (more complex)
    from operator import attrgetter
    sorted_flights = sorted(
        flights,
        key=lambda f: (f.carrid, f.connid)
    )

    return sorted_flights


# ==============================================================================
# Pattern 6: ABAP DELETE → Python list comprehension / filter()
# ==============================================================================

def example_delete_from_table(flights: List[Flight], carrier_to_remove: str) -> List[Flight]:
    """
    ABAP:
        DELETE lt_flights WHERE carrid = 'AA'.

    Python equivalent:
    """
    # Method 1: List comprehension (creates new list)
    flights = [f for f in flights if f.carrid != carrier_to_remove]

    # Method 2: In-place removal (less efficient for multiple items)
    flights_copy = flights.copy()
    for flight in flights_copy[:]:
        if flight.carrid == carrier_to_remove:
            flights_copy.remove(flight)

    return flights


# ==============================================================================
# Pattern 7: ABAP GROUP BY → Python itertools.groupby
# ==============================================================================

def example_group_by(flights: List[Flight]) -> Dict[str, List[Flight]]:
    """
    ABAP:
        DATA(lt_grouped) = VALUE tt_grouped(
          FOR GROUPS <group> OF <flight> IN lt_flights
          GROUP BY ( carrid = <flight>-carrid )
          ( carrid = <group>-carrid
            flights = VALUE #( FOR f IN GROUP <group> ( f ) ) )
        ).

    Python equivalent:
    """
    # Method 1: Using itertools.groupby (requires sorted data)
    flights_sorted = sorted(flights, key=lambda f: f.carrid)
    grouped = {
        carrier: list(group)
        for carrier, group in itertools.groupby(flights_sorted, key=lambda f: f.carrid)
    }

    # Method 2: Using dict comprehension
    carriers = set(f.carrid for f in flights)
    grouped_alt = {
        carrier: [f for f in flights if f.carrid == carrier]
        for carrier in carriers
    }

    return grouped


# ==============================================================================
# Pattern 8: ABAP REDUCE → Python reduce() / comprehensions
# ==============================================================================

def example_reduce_aggregation(flights: List[Flight]) -> float:
    """
    ABAP:
        DATA(lv_total) = REDUCE i(
          INIT sum = 0
          FOR flight IN lt_flights
          NEXT sum = sum + flight-price ).

    Python equivalent:
    """
    # Method 1: Using sum() (most Pythonic)
    total = sum(f.price for f in flights)

    # Method 2: Using reduce()
    from functools import reduce
    total_alt = reduce(lambda acc, f: acc + f.price, flights, 0)

    return total


# ==============================================================================
# Pattern 9: ABAP VALUE Constructor → Python list/dict comprehension
# ==============================================================================

def example_value_constructor(carrier_ids: List[str]) -> List[Flight]:
    """
    ABAP:
        DATA(lt_flights) = VALUE tt_flights(
          FOR carrier IN lt_carrier_ids
          ( carrid = carrier
            connid = '001'
            seatsmax = 200 )
        ).

    Python equivalent:
    """
    flights = [
        Flight(
            carrid=carrier,
            connid='001',
            fldate=date(2024, 1, 1),
            price=500.0,
            currency='USD',
            seatsmax=200,
            seatsocc=0
        )
        for carrier in carrier_ids
    ]

    return flights


# ==============================================================================
# Pattern 10: ABAP CORRESPONDING → Python dict unpacking
# ==============================================================================

@dataclass
class FlightSummary:
    """Simplified flight structure"""
    carrid: str
    connid: str
    price: float


def example_corresponding(flights: List[Flight]) -> List[FlightSummary]:
    """
    ABAP:
        DATA(lt_summary) = CORRESPONDING tt_summary(
          lt_flights MAPPING carrid = carrid connid = connid ).

    Python equivalent:
    """
    # Method 1: Manual field selection
    summaries = [
        FlightSummary(
            carrid=f.carrid,
            connid=f.connid,
            price=f.price
        )
        for f in flights
    ]

    # Method 2: Dynamic with shared fields
    summary_fields = set(FlightSummary.__dataclass_fields__.keys())
    summaries_alt = [
        FlightSummary(**{
            k: v for k, v in f.__dict__.items()
            if k in summary_fields
        })
        for f in flights
    ]

    return summaries


# ==============================================================================
# Complete Example: Complex Table Operations
# ==============================================================================

def complex_table_operations_example():
    """
    Demonstrates complex ABAP table operations translated to Python
    Mimics patterns from zcl_demo_abap_internal_tables
    """
    # Initialize sample data
    flights = [
        Flight('AA', '001', date(2024, 1, 15), 450.0, 'USD', 200, 150),
        Flight('AA', '002', date(2024, 1, 16), 480.0, 'USD', 180, 170),
        Flight('LH', '001', date(2024, 1, 15), 520.0, 'EUR', 220, 200),
        Flight('BA', '001', date(2024, 1, 17), 390.0, 'GBP', 150, 120),
    ]

    # Filter high-capacity flights
    large_flights = [f for f in flights if f.seatsmax >= 180]

    # Sort by carrier and connection
    large_flights.sort(key=lambda f: (f.carrid, f.connid))

    # Group by carrier and calculate totals
    flights_by_carrier = {}
    for flight in large_flights:
        if flight.carrid not in flights_by_carrier:
            flights_by_carrier[flight.carrid] = []
        flights_by_carrier[flight.carrid].append(flight)

    # Calculate revenue per carrier
    revenue_by_carrier = {
        carrier: sum(f.price * f.seatsocc for f in carrier_flights)
        for carrier, carrier_flights in flights_by_carrier.items()
    }

    print(f"Total carriers: {len(revenue_by_carrier)}")
    for carrier, revenue in revenue_by_carrier.items():
        print(f"{carrier}: ${revenue:,.2f}")

    return revenue_by_carrier


# ==============================================================================
# Performance Considerations
# ==============================================================================

def performance_tips():
    """
    Performance equivalents for ABAP table operations

    1. ABAP Sorted Tables → Python: Use bisect module or maintain sorted order
    2. ABAP Hashed Tables → Python: Use dict or set for O(1) lookups
    3. ABAP BINARY SEARCH → Python: Use bisect.bisect_left()
    4. Large datasets → Consider pandas DataFrame
    """
    import bisect

    # Example: Maintaining sorted table with binary search
    flights_sorted = []

    def insert_sorted(flight: Flight):
        """Insert maintaining sort order (like ABAP sorted table)"""
        bisect.insort(flights_sorted, flight, key=lambda f: f.carrid)

    # Example: Hash table for fast lookups (like ABAP hashed table)
    flights_dict: Dict[tuple, Flight] = {}

    def add_to_hash(flight: Flight):
        """Add to hash table with composite key"""
        key = (flight.carrid, flight.connid, flight.fldate)
        flights_dict[key] = flight

    def lookup_by_key(carrid: str, connid: str, fldate: date) -> Optional[Flight]:
        """O(1) lookup like ABAP hashed table READ"""
        return flights_dict.get((carrid, connid, fldate))


if __name__ == '__main__':
    print("ABAP Internal Tables to Python Migration Examples")
    print("=" * 60)
    complex_table_operations_example()
