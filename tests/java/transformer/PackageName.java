public class PackageName {
    void func() {
        staticB.staticFunc();
        var staticB = new staticB();

        staticC.scFunc();
        class staticC {
            static void scFunc() {

            }
        }
    }
}

class staticC {
    static void scFunc() {

    }
}