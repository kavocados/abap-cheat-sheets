/**
 * ABAP Internal Tables to TypeScript Collections Migration Example
 *
 * This module demonstrates how to migrate common ABAP internal table operations
 * to equivalent TypeScript code using modern JavaScript/TypeScript idioms.
 *
 * Based on: src/zcl_demo_abap_internal_tables.clas.abap
 * Reference: 01_Internal_Tables.md
 */

// ==============================================================================
// Data Models (ABAP Types → TypeScript interfaces/classes)
// ==============================================================================

/**
 * Equivalent to ABAP structure for flight data
 * ABAP: TYPES ty_flight TYPE zdemo_abap_fli
 */
interface Flight {
    carrid: string;      // Carrier ID
    connid: string;      // Connection ID
    fldate: Date;        // Flight date
    price: number;       // Price
    currency: string;    // Currency
    seatsmax: number;    // Maximum seats
    seatsocc: number;    // Occupied seats
}

/**
 * Equivalent to ABAP carrier structure
 * ABAP: TYPES ty_carrier TYPE zdemo_abap_carr
 */
interface Carrier {
    carrid: string;      // Carrier ID
    carrname: string;    // Carrier name
    currcode: string;    // Currency code
    url?: string;        // Website URL
}

// ==============================================================================
// Pattern 1: ABAP APPEND → TypeScript array.push()
// ==============================================================================

function exampleAppendOperation(): Flight[] {
    /**
     * ABAP:
     *   DATA lt_flights TYPE TABLE OF ty_flight.
     *   APPEND VALUE #( carrid = 'AA' connid = '001' ) TO lt_flights.
     *
     * TypeScript equivalent:
     */
    const flights: Flight[] = [];

    // Simple push
    flights.push({
        carrid: 'AA',
        connid: '001',
        fldate: new Date('2024-01-15'),
        price: 450.00,
        currency: 'USD',
        seatsmax: 200,
        seatsocc: 150
    });

    return flights;
}

// ==============================================================================
// Pattern 2: ABAP LOOP AT → TypeScript for...of / forEach
// ==============================================================================

function exampleLoopOperations(flights: Flight[]): Flight[] {
    /**
     * ABAP:
     *   LOOP AT lt_flights INTO DATA(ls_flight).
     *     IF ls_flight-seatsmax > 100.
     *       ls_flight-price = ls_flight-price * '1.1'.
     *       MODIFY lt_flights FROM ls_flight.
     *     ENDIF.
     *   ENDLOOP.
     *
     * TypeScript equivalent:
     */

    // Method 1: Direct modification (mutable)
    for (const flight of flights) {
        if (flight.seatsmax > 100) {
            flight.price = flight.price * 1.1;
        }
    }

    // Method 2: Functional approach (immutable)
    const updatedFlights = flights.map(flight =>
        flight.seatsmax > 100
            ? { ...flight, price: flight.price * 1.1 }
            : flight
    );

    return flights;
}

// ==============================================================================
// Pattern 3: ABAP READ TABLE → TypeScript find()
// ==============================================================================

function exampleReadTable(flights: Flight[], carrier: string): Flight | undefined {
    /**
     * ABAP:
     *   READ TABLE lt_flights INTO DATA(ls_flight)
     *     WITH KEY carrid = 'AA'.
     *
     * TypeScript equivalent:
     */

    // Method 1: Using find()
    const flight = flights.find(f => f.carrid === carrier);

    // Method 2: Using filter()[0]
    const flightAlt = flights.filter(f => f.carrid === carrier)[0];

    return flight;
}

// ==============================================================================
// Pattern 4: ABAP FILTER → TypeScript filter()
// ==============================================================================

function exampleFilterTable(flights: Flight[], minSeats: number = 150): Flight[] {
    /**
     * ABAP:
     *   DATA(lt_filtered) = FILTER #( lt_flights
     *     WHERE seatsmax > 150 ).
     *
     * TypeScript equivalent:
     */

    // Method 1: Using filter()
    const filtered = flights.filter(f => f.seatsmax > minSeats);

    // Method 2: With multiple conditions
    const filteredComplex = flights.filter(f =>
        f.seatsmax > minSeats &&
        f.seatsocc < f.seatsmax * 0.9
    );

    return filtered;
}

// ==============================================================================
// Pattern 5: ABAP SORT → TypeScript sort()
// ==============================================================================

function exampleSortTable(flights: Flight[]): Flight[] {
    /**
     * ABAP:
     *   SORT lt_flights BY carrid ASCENDING connid DESCENDING.
     *
     * TypeScript equivalent:
     */

    // Method 1: In-place sort
    flights.sort((a, b) => {
        // Sort by carrid ascending
        if (a.carrid < b.carrid) return -1;
        if (a.carrid > b.carrid) return 1;

        // Then by connid descending
        if (a.connid > b.connid) return -1;
        if (a.connid < b.connid) return 1;

        return 0;
    });

    // Method 2: Create new sorted array
    const sortedFlights = [...flights].sort((a, b) =>
        a.carrid.localeCompare(b.carrid) || b.connid.localeCompare(a.connid)
    );

    return sortedFlights;
}

// ==============================================================================
// Pattern 6: ABAP DELETE → TypeScript filter()
// ==============================================================================

function exampleDeleteFromTable(flights: Flight[], carrierToRemove: string): Flight[] {
    /**
     * ABAP:
     *   DELETE lt_flights WHERE carrid = 'AA'.
     *
     * TypeScript equivalent:
     */

    // Method 1: Using filter (creates new array)
    const remainingFlights = flights.filter(f => f.carrid !== carrierToRemove);

    // Method 2: In-place removal using splice (less efficient)
    for (let i = flights.length - 1; i >= 0; i--) {
        if (flights[i].carrid === carrierToRemove) {
            flights.splice(i, 1);
        }
    }

    return remainingFlights;
}

// ==============================================================================
// Pattern 7: ABAP GROUP BY → TypeScript reduce() / Map
// ==============================================================================

function exampleGroupBy(flights: Flight[]): Map<string, Flight[]> {
    /**
     * ABAP:
     *   DATA(lt_grouped) = VALUE tt_grouped(
     *     FOR GROUPS <group> OF <flight> IN lt_flights
     *     GROUP BY ( carrid = <flight>-carrid )
     *     ( carrid = <group>-carrid
     *       flights = VALUE #( FOR f IN GROUP <group> ( f ) ) )
     *   ).
     *
     * TypeScript equivalent:
     */

    // Method 1: Using reduce()
    const grouped = flights.reduce((acc, flight) => {
        const carrier = flight.carrid;
        if (!acc.has(carrier)) {
            acc.set(carrier, []);
        }
        acc.get(carrier)!.push(flight);
        return acc;
    }, new Map<string, Flight[]>());

    // Method 2: Using for...of
    const groupedAlt = new Map<string, Flight[]>();
    for (const flight of flights) {
        const carrier = flight.carrid;
        if (!groupedAlt.has(carrier)) {
            groupedAlt.set(carrier, []);
        }
        groupedAlt.get(carrier)!.push(flight);
    }

    // Method 3: Using lodash (if available)
    // import { groupBy } from 'lodash';
    // const groupedLodash = groupBy(flights, 'carrid');

    return grouped;
}

// ==============================================================================
// Pattern 8: ABAP REDUCE → TypeScript reduce()
// ==============================================================================

function exampleReduceAggregation(flights: Flight[]): number {
    /**
     * ABAP:
     *   DATA(lv_total) = REDUCE i(
     *     INIT sum = 0
     *     FOR flight IN lt_flights
     *     NEXT sum = sum + flight-price ).
     *
     * TypeScript equivalent:
     */

    // Method 1: Using reduce()
    const total = flights.reduce((sum, flight) => sum + flight.price, 0);

    // Method 2: Using for...of
    let totalAlt = 0;
    for (const flight of flights) {
        totalAlt += flight.price;
    }

    return total;
}

// ==============================================================================
// Pattern 9: ABAP VALUE Constructor → TypeScript map()
// ==============================================================================

function exampleValueConstructor(carrierIds: string[]): Flight[] {
    /**
     * ABAP:
     *   DATA(lt_flights) = VALUE tt_flights(
     *     FOR carrier IN lt_carrier_ids
     *     ( carrid = carrier
     *       connid = '001'
     *       seatsmax = 200 )
     *   ).
     *
     * TypeScript equivalent:
     */

    const flights = carrierIds.map(carrier => ({
        carrid: carrier,
        connid: '001',
        fldate: new Date('2024-01-01'),
        price: 500.0,
        currency: 'USD',
        seatsmax: 200,
        seatsocc: 0
    }));

    return flights;
}

// ==============================================================================
// Pattern 10: ABAP CORRESPONDING → TypeScript destructuring/mapping
// ==============================================================================

interface FlightSummary {
    carrid: string;
    connid: string;
    price: number;
}

function exampleCorresponding(flights: Flight[]): FlightSummary[] {
    /**
     * ABAP:
     *   DATA(lt_summary) = CORRESPONDING tt_summary(
     *     lt_flights MAPPING carrid = carrid connid = connid ).
     *
     * TypeScript equivalent:
     */

    // Method 1: Manual field selection
    const summaries = flights.map(f => ({
        carrid: f.carrid,
        connid: f.connid,
        price: f.price
    }));

    // Method 2: Using destructuring
    const summariesAlt = flights.map(({ carrid, connid, price }) => ({
        carrid,
        connid,
        price
    }));

    return summaries;
}

// ==============================================================================
// Complete Example: Complex Table Operations
// ==============================================================================

function complexTableOperationsExample(): Map<string, number> {
    /**
     * Demonstrates complex ABAP table operations translated to TypeScript
     * Mimics patterns from zcl_demo_abap_internal_tables
     */

    // Initialize sample data
    const flights: Flight[] = [
        { carrid: 'AA', connid: '001', fldate: new Date('2024-01-15'), price: 450.0, currency: 'USD', seatsmax: 200, seatsocc: 150 },
        { carrid: 'AA', connid: '002', fldate: new Date('2024-01-16'), price: 480.0, currency: 'USD', seatsmax: 180, seatsocc: 170 },
        { carrid: 'LH', connid: '001', fldate: new Date('2024-01-15'), price: 520.0, currency: 'EUR', seatsmax: 220, seatsocc: 200 },
        { carrid: 'BA', connid: '001', fldate: new Date('2024-01-17'), price: 390.0, currency: 'GBP', seatsmax: 150, seatsocc: 120 },
    ];

    // Filter high-capacity flights
    const largeFlights = flights.filter(f => f.seatsmax >= 180);

    // Sort by carrier and connection
    largeFlights.sort((a, b) =>
        a.carrid.localeCompare(b.carrid) || a.connid.localeCompare(b.connid)
    );

    // Group by carrier
    const flightsByCarrier = largeFlights.reduce((acc, flight) => {
        if (!acc.has(flight.carrid)) {
            acc.set(flight.carrid, []);
        }
        acc.get(flight.carrid)!.push(flight);
        return acc;
    }, new Map<string, Flight[]>());

    // Calculate revenue per carrier
    const revenueByCarrier = new Map<string, number>();
    for (const [carrier, carrierFlights] of flightsByCarrier.entries()) {
        const revenue = carrierFlights.reduce(
            (sum, f) => sum + (f.price * f.seatsocc),
            0
        );
        revenueByCarrier.set(carrier, revenue);
    }

    console.log(`Total carriers: ${revenueByCarrier.size}`);
    for (const [carrier, revenue] of revenueByCarrier.entries()) {
        console.log(`${carrier}: $${revenue.toLocaleString('en-US', { minimumFractionDigits: 2 })}`);
    }

    return revenueByCarrier;
}

// ==============================================================================
// Performance Considerations
// ==============================================================================

/**
 * Performance equivalents for ABAP table operations
 *
 * 1. ABAP Sorted Tables → TypeScript: Maintain sorted array or use binary search
 * 2. ABAP Hashed Tables → TypeScript: Use Map or Set for O(1) lookups
 * 3. ABAP BINARY SEARCH → TypeScript: Implement binary search or use libraries
 * 4. Large datasets → Consider using specialized libraries like lodash
 */

class SortedFlightTable {
    private flights: Flight[] = [];

    /**
     * Insert maintaining sort order (like ABAP sorted table)
     */
    insertSorted(flight: Flight): void {
        const index = this.flights.findIndex(f => f.carrid > flight.carrid);
        if (index === -1) {
            this.flights.push(flight);
        } else {
            this.flights.splice(index, 0, flight);
        }
    }

    /**
     * Binary search for flights by carrier
     */
    findByCarrier(carrid: string): Flight | undefined {
        let left = 0;
        let right = this.flights.length - 1;

        while (left <= right) {
            const mid = Math.floor((left + right) / 2);
            const midCarrier = this.flights[mid].carrid;

            if (midCarrier === carrid) {
                return this.flights[mid];
            } else if (midCarrier < carrid) {
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }

        return undefined;
    }
}

/**
 * Hash table for fast lookups (like ABAP hashed table)
 */
class HashedFlightTable {
    private flightsMap = new Map<string, Flight>();

    /**
     * Add to hash table with composite key
     */
    add(flight: Flight): void {
        const key = `${flight.carrid}|${flight.connid}|${flight.fldate.toISOString()}`;
        this.flightsMap.set(key, flight);
    }

    /**
     * O(1) lookup like ABAP hashed table READ
     */
    lookupByKey(carrid: string, connid: string, fldate: Date): Flight | undefined {
        const key = `${carrid}|${connid}|${fldate.toISOString()}`;
        return this.flightsMap.get(key);
    }
}

// ==============================================================================
// Main Execution
// ==============================================================================

if (require.main === module) {
    console.log('ABAP Internal Tables to TypeScript Migration Examples');
    console.log('='.repeat(60));
    complexTableOperationsExample();
}

export {
    Flight,
    Carrier,
    FlightSummary,
    exampleAppendOperation,
    exampleLoopOperations,
    exampleReadTable,
    exampleFilterTable,
    exampleSortTable,
    exampleDeleteFromTable,
    exampleGroupBy,
    exampleReduceAggregation,
    exampleValueConstructor,
    exampleCorresponding,
    complexTableOperationsExample,
    SortedFlightTable,
    HashedFlightTable
};
