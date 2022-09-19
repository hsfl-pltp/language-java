import java.io.*;

class TryResources {

    final FileReader f1 = null;
    final FileReader f2 = null;

    static String readFirstLineFromFile(String path) throws IOException {
	    try (FileReader fr = new FileReader(path);
	         final BufferedReader br = new BufferedReader(fr)) {
	        return br.readLine();
	    }
	}

    void foo() throws IOException {
        try (this.f1; ) {}
        try (this.f1) {}
        try (f1) {}
        try (f1; ) {}
        try (this.f1; f2 ) {}
        try (this.f1; this.f2; ) {}
        try (this.f1; this.f2; FileReader fr = new FileReader("blub.txt")) {}
    }
}
