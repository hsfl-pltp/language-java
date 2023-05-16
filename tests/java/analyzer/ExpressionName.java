public class TestA {

    TestA field = new TestA();
    public void func1(){
    }

    public void func2(TestA formalParam){
        formalParam.func1();
        field.func1();
    }

}