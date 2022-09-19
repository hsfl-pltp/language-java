import java.util.*;
import java.util.function.*;

public class MethodRefs {
    void foo(Supplier<HashSet<String>> f) {}

    void main() {
        foo(HashSet::new);
    }
}
