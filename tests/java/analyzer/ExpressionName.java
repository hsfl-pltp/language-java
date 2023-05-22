public class TestA {

    TestB field = new TestB();

    public TestA (TestB consParam){
        consParam.funcB();
    }

    public void funcA(TestB formalParam){
        formalParam.funcB();
        field.funcB();

        try {
            field.throwsBlewIt();
        } catch (BlewIt b) {
            b.getMessage();
        }
    }


}