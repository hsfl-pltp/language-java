enum Day {
    MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY
}

public class CaseTest {
    void switchStatementOld(Day day) {
        switch (day) {
            case MONDAY, FRIDAY, SUNDAY:
                System.out.println("hello");
                System.out.println(6);
            case TUESDAY: {
                System.out.println(7);
            }
            case THURSDAY, SATURDAY: System.out.println(8);
            default: System.out.println(9);
        }
    }
    void switchStatement(Day day) {
        switch (day) {
            case MONDAY, FRIDAY, SUNDAY ->
                System.out.println(6);
            case TUESDAY                -> {
                System.out.println(7);
            }
            case THURSDAY, SATURDAY -> System.out.println(8);
            default -> System.out.println(9);
        }
    }

    void switchExpression(Day day) {
        System.out.println(switch (day) {
            case MONDAY, FRIDAY, SUNDAY -> 6;
            case TUESDAY                -> 7;
            case THURSDAY, SATURDAY     -> 8;
            default              -> 9;
        });
    }
}
