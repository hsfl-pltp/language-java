public class TestA {

    TestB field = new TestB();

    public TestA(TestB consParam) {
        consParam.funcB();
    }

    public void funcA(TestB formalParam) {
        formalParam.funcB();
        field.funcB();

        try {
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
}