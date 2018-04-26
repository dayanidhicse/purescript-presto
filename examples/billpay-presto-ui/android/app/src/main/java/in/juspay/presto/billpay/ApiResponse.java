package in.juspay.presto.billpay;

/**
 * Created by dilipjain on 4/25/18.
 */

public class ApiResponse {

    int statusCode;
    byte[] data;

    public ApiResponse() {

    }

    public ApiResponse(int code, byte[] respData) {
        this.statusCode = code;
        this.data = respData;
    }

    public int getStatusCode() {
        return statusCode;
    }

    public void setStatusCode(int statusCode) {
        this.statusCode = statusCode;
    }

    public byte[] getData() {
        return data;
    }

    public void setData(byte[] data) {
        this.data = data;
    }
}
