
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

        Enum.High.enumFunc();

        Record.staticRecordFunc();

    }

    static TypeName myself() {
        return new TypeName();
    }

    interface Interface {
        TypeName tn = TypeName.myself();
    }

}

