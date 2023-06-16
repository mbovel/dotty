
import language.experimental.clauseInterleaving
import language.experimental.setNotation


/** This is a test for qualified types trying to find the edges of
 *  what is possible.
 *  It should not be taken as the idiomatic way to use this feature !
 *
 *
 *  Json schemas taken from https://cchandurkar.github.io/json-schema-to-case-class/
 *  On the 2023-06-13
 */


/** qualifier-heavy representation of json objects */
type Dict = Map[String, Any]

/** case-class representation of json objects
 *  @param rest stores key-value pairs not specified in the schema
 */
trait Obj(val rest: Dict = Map())

extension (d: Dict)
  def hasFieldOfType(field: String)[T]: Boolean =
    d.contains(field) && d(field).isInstanceOf[T]

  def optionalFieldOfType(field: String)[T]: Boolean =
    if d.contains(field) then d(field).isInstanceOf[T] else true


def unique(a: List[?]): Boolean = a.length == a.distinct.length

def required(using d: Dict)(field: String)[T]: Boolean = d.hasFieldOfType(field)[T]

def optional(using d: Dict)(field: String)[T]: Boolean = d.optionalFieldOfType(field)[T]

/*
// This should be equivalent to the above with T' = {x: T with pred(x)}
def optionalWithPred(using d: Dict)(field: String)[T](pred: T => Boolean): Boolean =
  if d.contains(field) then
    d(field).isInstanceOf[T] &&
    pred(d(field).asInstanceOf[T])
  else true
*/


// SimpleSchema

type Person = {d: Dict with
  d.optionalFieldOfType("firstName")[String] &&
  d.optionalFieldOfType("lastName")[String] &&
  d.optionalFieldOfType("age")[{x: Int with x > 0}]
}

// NestedSchema

type Product = {d: Dict with
  def required(f: String)[T] = d.hasFieldOfType(f)[T]
  def optional(f: String)[T] = d.optionalFieldOfType(f)[T]
  required("productId")[Int] &&
  required("productName")[String] &&
  required("price")[{x: Double with x > 0}] &&
  optional("tags")[{a: List[String] with a.size >= 1 && unique(a)}] &&
  optional("dimensions")[Dimensions]
}

type Dimensions = {d: Dict with
  def required(f: String)[T] = d.hasFieldOfType(f)[T]
  required("length")[Double] &&
  required("width")[Double] &&
  required("height")[Double]
}

// Schema with local references

type Address = {d: Dict with
  given Dict = d
  required("street_adress")[String] &&
  required("city")[String] &&
  required("state")[String] &&
  optional("zip")[Int]
}

type Unnamed1 = {d: Dict with
  given Dict = d
  optional("billing_address")[Address] && // Maybe should be opaque aliases of Address
  optional("shipping_address")[Address]
}

// Schema Validation
object usingCaseClass:

  case class LongitudeAndLatitudeValues(
    latitude: Double with -90 <= latitude && latitude <= 90,
    longitude: Double with -180 <= longitude && longitude <= 180,
    city: String with raw"^[A-Za-z . ,'-]+$$".r.matches(city)
  ) extends Obj

object usingImplicits:
  type LongitudeAndLatitudeValues = {d: Dict with
    given Dict = d
    required("latitude")[{l: Double with -90 <= l && l <= 90}] &&
    required("longitude")[{l: Double with -180 <= l && l <= 180}] &&
    required("city")[{city: String with raw"^[A-Za-z . ,'-]+$$".r.matches(city)}]
  }

// Enumerations - String

type Person2 = {d: Dict with
  given Dict = d
  required("personId")[Int] &&
  required("transactions")[List[Transaction]]
}

type Transaction = {d: Dict with
  given Dict = d
  optional("txId")[Int] &&
  optional("txTime")[Int] &&
  optional("txType")[{txType: String with
    // TODO: Update below when dotty#17939 is fixed
    true //List("DEBIT", "CREDIT", "VOID").contains(txType)
  }]
}

// Enumeration - Array

type Person3 = {d: Dict with
  given Dict = d
  required("numbers")[List[{
    s: String with
      // TODO: Update below when dotty#17939 is fixed
      true //List("one", "two", "three").contains(s)
  }]]
}

// Schema Composition - allOf (Simple)

type Person4 = {d: Dict with
  given Dict = d
  optional("firstName")[String] &&
  optional("age")[{x: Int with
    x >= 3 &&
    x%3 == 0 &&
    x%5 == 0
  }]
}
