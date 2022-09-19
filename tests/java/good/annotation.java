
@interface TestAnn1 {}
@interface TestAnn2 {
  boolean value() default false;
}
@interface TestAnn3 {
  String first();
  String last();
}

@interface Property {}

@interface ForAll {}

class Other {
  @TestAnn1
  void Test1() {}

  @TestAnn2(true)
  void Test2() {}

  @TestAnn3(first = "foo", last = "bar")
  void Test3() {}
}

class C {
  @Property boolean reverseTwiceIsOriginal(@ForAll java.util.List<Integer> original) {
    return true;
  }
}
