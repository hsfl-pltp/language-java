record Circle() implements Shape {}
record Rectangle(int sealed, double permits) implements Shape {} // sealed and permits are no keywords here
sealed interface Shape permits Circle, Rectangle {} // but here
