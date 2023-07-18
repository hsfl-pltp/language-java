import package2.ClassFromPackage2;
public class TypeName {

    public static void staticFunc() {

    }

    public void func() {

        TypeName.staticFunc();
        var TypeName = 0;
        class Foo {
            static void funcFoo() {

            }
        }
        Foo.funcFoo();
        ClassFromPackage2.func();
    }


    static class InnerClass {
        static void staticClassFunc() {
        }
    }

    enum Enum {
        High;

        void enumFunc() {

        }

        static void staticEnumFunc() {

        }
    }

    record Record(String name) {

        static void staticRecordFunc() {

        }
    }


    void func3() {
        InnerClass.staticClassFunc();
        Enum.staticEnumFunc();

        Record.staticRecordFunc();

        staticC.scFunc();
        class staticC {
            static void scFunc() {

            }
        }

    }

    static TypeName myself() {
        return new TypeName();
    }

    interface Interface {
        TypeName tn = TypeName.myself();
    }

}

class staticC {
    static void scFunc() {

    }
}

class MultiNameTypeName {
    void func() {
        outerClass.innerA.func();//TypeName
    }
}

class outerClass {
    static TestB fieldOfOuter = new TestB();

    class innerA {
        static void func() {
            innerB.innerInnerB.func();//TypeName
        }
    }

    class innerB {
        class innerInnerB {
            static void func() {

            }
        }
    }
}