record Point2D(double x, double y) {
    public double sum() {
        return x + y;
    }
}

interface Foo {}
interface Bar {}

record Point3D(double x, double y, double z) implements Foo, Bar {}
