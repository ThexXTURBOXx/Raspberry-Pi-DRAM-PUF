public class DramPufJni {

    static {
        System.loadLibrary("SerialReader");
    }

    public static native String genKey(String serialPort, String gpioChip,
                                       int baud, int rpiPowerPort, int sleep,
                                       String[] params, int paramsSize,
                                       String posFile, int keySize);

    public static String genKey(String serialPort, String gpioChip,
                                int baud, int rpiPowerPort, int sleep,
                                String[] params, String posFile, int keySize) {
        return genKey(serialPort, gpioChip, baud, rpiPowerPort, sleep, params, params.length, posFile, keySize);
    }

    public static void main(String[] args) {
        // Example parameters
        String[] params = new String[]{"0", "0", "0", "C3", "C38", "00000000", "0", "0", "120"};
        String key = genKey("/dev/ttyS0", "gpiochip0", 115200, 2, 5, params, "stable.pos", 1024);
        System.out.println("Generated key: " + key);
    }

}
