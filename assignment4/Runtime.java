import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.System;

public class Runtime {
    static BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));

    public static String plusString(String a, String b) {
        return a + b;
    }

    public static int stringCompare(String a, String b) {
        return a.compareTo(b);
    }

    public static int readInt() throws IOException {
        String line = reader.readLine();

        return Integer.parseInt(line);
    }

    public static double readDouble() throws IOException {
        String line = reader.readLine();

        return Double.parseDouble(line);
    }

    public static String readString() throws IOException {
        return reader.readLine();
    }

    public static void printInt(int i) {
        System.out.println(i);
    }

    public static void printDouble(double d) {
        System.out.println(d);
    }

    public static void printString(String s) {
        System.out.println(s);
    }
}