public class TestA {

    TestB field = new TestB();

    public TestA(TestB consParam) {
        consParam.funcB();
    }

    public void funcA(TestB formalParam) {
        formalParam.funcB();
        field.funcB();
        TestB a = new TestB(), b = a.myself();
        TestB a1 = field.myself(), a2 = a1.myself();

        try (TestB tryTestB = new TestB()){
            tryTestB.funcB();
            field.throwsBlewIt();
        } catch (BlewIt b) {
            b.getMessage();
        }
    }

    public void funcLocalVars() {
        var local1 = new TestB();
        TestB local2 = new TestB(), local3 = new TestB();
        local1.funcB();
        local2.funcB();
        local3.funcB();
    }

    public void funcForLoop(){
        var arrayB = new TestB[1];
        for (TestB varFor = new TestB(); varFor.returnBool(); varFor.funcB() ){
            varFor.funcB();
        }
        for (TestB varEnhancedFor : arrayB){
            varEnhancedFor.funcB();
        }
    }


}

class MultiNameExpressionName {
    void func() {
        outer.fieldOfOuter.funcB(); //ExpressionName

        var node = new Node();
        node.next.objectFunc(); //ExpressionName
        node.next.last.next.next.objectFunc(); //ExpressionName

        Node.staticSelfField.staticFunc();//ExpressionName
        Node.staticSelfField.staticSelfField.staticFunc();//ExpressionName

        EnumType.High.enumFunc();//ExpressionName
    }
}

class outer {
    static TestB fieldOfOuter = new TestB();


}

class Node {
    Node next = new Node();
    Node last = new Node();

    static Node staticSelfField = new Node();

    static void staticFunc() {

    }

    void objectFunc() {

    }
}

enum EnumType {
    High;

    void enumFunc() {

    }
}
