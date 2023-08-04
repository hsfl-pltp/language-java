import Language.Java.Syntax;
public class PackageName {
    void func() {
        staticB.staticFunc();
        var staticB = new staticB();

        System.out.println("qualified use");
        Syntax.Exp.func();
    }

}