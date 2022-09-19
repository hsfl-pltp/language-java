class InstanceofTest {

    static void bar(String s) {}

    static void foo(Object obj) {
        if (obj instanceof String s) {
            bar(s);
        }
    }
}
