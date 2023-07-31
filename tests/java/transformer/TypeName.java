import ImportedPackage.ImportedClass;
public class TypeName {

    public void func() {
        //test for call of static function
        TypeName.staticFunc();

        //test for influence of declaration order
        var TypeName = 0;


        // test for static function call of class declarated in methodbody
        class Foo {
            static void funcFoo() {

            }
        }
        Foo.funcFoo();

        // test for function call from imported class
        ImportedClass.func();

        // test for static function from inner class
        InnerClass.staticClassFunc();

        //test for static function from enum class
        Enum.staticEnumFunc();

        //test for static function from record
        Record.staticRecordFunc();


    }

    public static void staticFunc() {

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
        

    }

    static TypeName myself() {
        return new TypeName();
    }

    interface Interface {
        TypeName tn = TypeName.myself();
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