
import language.experimental.clauseInterleaving
import language.experimental.postfixLambda


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

type Person = Dict with
  d =>
    d.optionalFieldOfType("firstName")[String] &&
    d.optionalFieldOfType("lastName")[String] &&
    d.optionalFieldOfType("age")[Int with _ > 0]

// NestedSchema

type Product = Dict with
  d =>
    def required(f: String)[T] = d.hasFieldOfType(f)[T]
    def optional(f: String)[T] = d.optionalFieldOfType(f)[T]
    required("productId")[Int] &&
    required("productName")[String] &&
    required("price")[Double with _ > 0] &&
    optional("tags")[List[String] with (a => a.size >= 1 && unique(a))] &&
    optional("dimensions")[Dimensions]

type Dimensions = Dict with
  d =>
    def required(f: String)[T] = d.hasFieldOfType(f)[T]
    required("length")[Double] &&
    required("width")[Double] &&
    required("height")[Double]

// Schema with local references

type Address = Dict with
  d =>
    given Dict = d
    required("street_adress")[String] &&
    required("city")[String] &&
    required("state")[String] &&
    optional("zip")[Int]

type Unnamed1 = Dict with
  d =>
    given Dict = d
    optional("billing_address")[Address] && // Maybe should be opaque aliases of Address
    optional("shipping_address")[Address]

// Schema Validation
object usingCaseClass:

  case class LongitudeAndLatitudeValues(
    latitude: Double with (latitude => -90 <= latitude && latitude <= 90),
    longitude: Double with (longitude => -180 <= longitude && longitude <= 180),
    city: String with (city => raw"^[A-Za-z . ,'-]+$$".r.matches(city))
  ) extends Obj

object usingImplicits:
  type LongitudeAndLatitudeValues = Dict with
    d =>
      given Dict = d
      required("latitude")[Double with (l => -90 <= l && l <= 90)] &&
      required("longitude")[Double with (l => -180 <= l && l <= 180)] &&
      required("city")[String with (city => raw"^[A-Za-z . ,'-]+$$".r.matches(city))]

// Enumerations - String

type Person2 = Dict with
  d =>
    given Dict = d
    required("personId")[Int] &&
    required("transactions")[List[Transaction]]

type Transaction = Dict with
  d =>
    given Dict = d
    optional("txId")[Int] &&
    optional("txTime")[Int] &&
    optional("txType")[String with txType =>
      // TODO: Update below when dotty#17939 is fixed
      true //List("DEBIT", "CREDIT", "VOID").contains(txType)
    ]

// Enumeration - Array

type Person3 = Dict with
  d =>
    given Dict = d
    required("numbers")[List[
      String with s =>
        // TODO: Update below when dotty#17939 is fixed
        true //List("one", "two", "three").contains(s)
    ]]

// Schema Composition - allOf (Simple)

type Person4 = Dict with
  d =>
    given Dict = d
    optional("firstName")[String] &&
    optional("age")[Int with x =>
      x >= 3 &&
      x%3 == 0 &&
      x%5 == 0
    ]
