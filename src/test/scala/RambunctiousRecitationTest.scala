import org.scalatest.FlatSpec

class RambunctiousRecitationTest extends FlatSpec{

  "The 2020th number spoken for the init numbers 0,3,6 " should " be 436 " in {
    assert(RambunctiousRecitation.get2020th(List(0,3,6), 2020) == 436)
  }

  "The 2020th number spoken for the init numbers 1,3,2 " should " be 1 " in {
    assert(RambunctiousRecitation.get2020th(List(1,3,2), 2020) == 1)
  }

  "The 2020th number spoken for the init numbers 1,2,3 " should " be 10 " in {
    assert(RambunctiousRecitation.get2020th(List(2,1,3), 2020) == 10)
  }

  "The 2020th number spoken for the init numbers 2,3,1 " should " be 78 " in {
    assert(RambunctiousRecitation.get2020th(List(2,3,1), 2020) == 78)
  }

  "The 2020th number spoken for the init numbers 3,2,1 " should " be 438 " in {
    assert(RambunctiousRecitation.get2020th(List(3,2,1), 2020) == 438)
  }

  "The 2020th number spoken for the init numbers 3,1,2 " should " be 1836 " in {
    assert(RambunctiousRecitation.get2020th(List(3,1,2), 2020) == 1836)
  }

  "The 2020th number spoken for the init numbers 10,16,6,0,1,17 " should " be 412 " in {
    assert(RambunctiousRecitation.get2020th(List(10,16,6,0,1,17), 2020) == 412)
  }

  "The 30000000th number spoken for the init numbers 10,16,6,0,1,17 " should " be 243 " in {
    assert(RambunctiousRecitation.get2020th(List(10,16,6,0,1,17), 30000000) == 243)
  }
}
