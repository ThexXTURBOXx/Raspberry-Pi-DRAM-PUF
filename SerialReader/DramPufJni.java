public class DramPufJni {

    static {
        System.loadLibrary("SerialReader");
    }

    public static native String genKey(String serialPort, int baud,
                                       int rpiPowerPort, int sleep,
                                       String[] params, int paramsSize,
                                       String posFile, int keySize);

}