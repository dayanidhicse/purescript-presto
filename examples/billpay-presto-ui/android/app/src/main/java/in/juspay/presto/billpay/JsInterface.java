package in.juspay.presto.billpay;

import android.Manifest;
import android.content.Context;
import android.content.pm.PackageManager;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.os.AsyncTask;
import android.os.Build;
import android.support.v4.app.ActivityCompat;
import android.support.v4.content.ContextCompat;
import android.util.Base64;
import android.util.Log;
import android.util.TypedValue;
import android.view.View;
import android.view.inputmethod.InputMethodManager;
import android.webkit.JavascriptInterface;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import in.juspay.mystique.DynamicUI;

/**
 * Created by dilipjain on 4/25/18.
 */

public class JsInterface {
    private MainActivity activity;
    private DynamicUI dynamicUI;
    private final static String LOG_TAG = JsInterface.class.getName();
    private final int PHONE_STATE_PERMISSION_CODE = 2;
    private String permissionCallback;


    public JsInterface(MainActivity activity, DynamicUI dynamicUI) {
        this.activity = activity;
        this.dynamicUI = dynamicUI;
    }

    @JavascriptInterface
    public void hideKeyboard() {
        View view = activity.getCurrentFocus();
        if (view != null) {
            InputMethodManager imm = (InputMethodManager) activity.getApplicationContext().getSystemService(Context.INPUT_METHOD_SERVICE);
            imm.hideSoftInputFromWindow(activity.getCurrentFocus().getWindowToken(), 0);
        }
    }

    @JavascriptInterface
    public String getDeviceDetails() {
        try {
            JSONObject deviceDetails = new JSONObject();

            deviceDetails
                    .put("deviceId", "")
                    .put("packageName", BuildConfig.APPLICATION_ID)
                    .put("os", "Android")
                    .put("model", Build.MODEL)
                    .put("version", Build.VERSION.SDK_INT)
                    .put("manufacturer", Build.MANUFACTURER);

            return deviceDetails.toString();
        } catch (Exception e) {
            return null;
        }
    }

    @JavascriptInterface
    public void trackEvent(String info, String screenName) {

    }

    @JavascriptInterface
    public void exitApp() {
        activity.finish();
    }


    @JavascriptInterface
    public void setClickFeedback(final String res) {
        this.activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                try {
                    final Integer resInt = Integer.valueOf(res);
                    View view = activity.findViewById(resInt);
                    TypedValue outValue = new TypedValue();
                    activity.getTheme().resolveAttribute(R.attr.selectableItemBackground, outValue, true);
                    //noinspection deprecation
                    if (view != null) {
                        view.setBackgroundResource(outValue.resourceId);
                    } else {
                    }
                } catch (Exception e) {
                    //No op
                }
            }
        });

    }

    String jsPermissionToAndroid(String permission) throws JSONException {
        JSONObject object = new JSONObject(permission);
        String p = object.getString("tag");

        switch (p) {
            case "PermissionReadPhoneState":
                return Manifest.permission.READ_PHONE_STATE;

            case "PermissionSendSms":
                return Manifest.permission.SEND_SMS;

            case "PermissionReadStorage":
                return Manifest.permission.READ_EXTERNAL_STORAGE;

            case "PermissionWriteStorage":
                return Manifest.permission.WRITE_EXTERNAL_STORAGE;

            default:
                throw new JSONException("Unrecognised permission type: " + p);
        }
    }

    @android.webkit.JavascriptInterface
    public void checkPermission(final String permission, final String callback) {
        try {
            String jPermission = jsPermissionToAndroid(permission);
            dynamicUI.addJsToWebView("window.callUICallback(\"" + callback + "\", " + getPermission(jPermission) + ")");
        } catch (Exception e) {
            dynamicUI.addJsToWebView("window.callUICallback(\"" + callback + "\", \"EXCEPTION\")");
        }
    }

    public String decodeBase64(String data) throws UnsupportedEncodingException {
        byte[] dataText = Base64.decode(data, Base64.DEFAULT);
        String text = new String(dataText, "UTF-8");
        return text;
    }

    @JavascriptInterface
    public boolean isNetworkAvailable() {
        ConnectivityManager connectivityManager
                = (ConnectivityManager) activity.getSystemService(Context.CONNECTIVITY_SERVICE);
        NetworkInfo activeNetworkInfo = connectivityManager.getActiveNetworkInfo();
        return activeNetworkInfo != null && activeNetworkInfo.isConnected();
    }

    @JavascriptInterface
    public boolean getPermission(final String permission) throws JSONException {
        if (ContextCompat.checkSelfPermission(activity, permission) == PackageManager.PERMISSION_DENIED) {
            return false;
        } else {
            return true;
        }
    }

    @android.webkit.JavascriptInterface
    public void setPermissions(final String permissionsJson, final String callback) throws JSONException {
        JSONArray permissions = new JSONArray(permissionsJson);
        final List<String> jPermissions = new ArrayList<>();

        for (int i = 0; i < permissions.length(); i++)
            if (ContextCompat.checkSelfPermission(activity, permissions.getString(i)) == PackageManager.PERMISSION_DENIED)
                jPermissions.add(permissions.getString(i));

        final String[] permissionList = new String[jPermissions.size()];
        permissionCallback = callback;
        ActivityCompat.requestPermissions(activity, jPermissions.toArray(permissionList), PHONE_STATE_PERMISSION_CODE);
    }

    public void onRequestPermissionsResult(int requestCode, String[] permissions, int[] grantResults) {
        if (requestCode == PHONE_STATE_PERMISSION_CODE) {
            try {
                JSONArray status = new JSONArray();
                for (int r : grantResults)
                    status.put(r == PackageManager.PERMISSION_GRANTED);
                dynamicUI.addJsToWebView("window.callUICallback(\"" + permissionCallback + "\", " + status.toString() + ");");
            } catch (Exception e) {
                dynamicUI.addJsToWebView("window.callUICallback(\"" + permissionCallback + "\", \"ERROR\");");
            }
        }
    }

    @JavascriptInterface
    public void callAPI(final String method, final String url, final String data, final String headers, boolean isSSLPinned, final String callback) {
        AsyncTask asyncTask = new AsyncTask() {
            @Override
            protected void onPostExecute(Object o) {
                if (o != null) {
                    Log.e(LOG_TAG, "Please check if HTTP method (GET, POST, ..) is supported");
                    ApiResponse apiResponse = (ApiResponse) o;
                    if (apiResponse.getStatusCode() == -1) {
                        String base64Data = Base64.encodeToString("{}".getBytes(), Base64.NO_WRAP);
                        String javascript = String.format("window.callUICallback('%s','%s','%s','%s','%s');", callback, "failure", base64Data, apiResponse.getStatusCode(), Base64.encode(url.getBytes(), Base64.NO_WRAP));
                        dynamicUI.addJsToWebView(javascript);
                    } else {
                        String base64Data = null;
                        if (apiResponse.getData() == null) {
                            base64Data = "";
                        } else {
                            base64Data = Base64.encodeToString(apiResponse.getData(), Base64.NO_WRAP);
                        }
                        String javascript = String.format("window.callUICallback('%s','%s','%s','%s','%s');", callback, "success", base64Data, apiResponse.getStatusCode(), Base64.encode(url.getBytes(), Base64.NO_WRAP));
                        dynamicUI.addJsToWebView(javascript);
                    }
                } else {
                    String base64Data = Base64.encodeToString("{}".getBytes(), Base64.NO_WRAP);
                    String javascript = String.format("window.callUICallback('%s','%s','%s','%s','%s');", callback, "failure", base64Data, -1, Base64.encode(url.getBytes(), Base64.NO_WRAP));
                    dynamicUI.addJsToWebView(javascript);
                }

            }

            @Override
            protected ApiResponse doInBackground(Object[] params) {
                HashMap<String, String> h = new HashMap<String, String>();
                try {
                    JSONObject jsonHeaders = new JSONObject(headers);
                    Iterator<?> keys = jsonHeaders.keys();
                    while (keys.hasNext()) {
                        String key = (String) keys.next();
                        String value = jsonHeaders.getString(key);
                        h.put(key, value);
                    }
                    if ("GET".equals(method)) {
                        HashMap<String, String> d = new HashMap<String, String>();
                        JSONObject jsonData = new JSONObject(data);
                        keys = jsonData.keys();
                        while (keys.hasNext()) {
                            String key = (String) keys.next();
                            String value = jsonHeaders.getString(key);
                            d.put(key, value);
                        }
                        return RestClient.get(url, d, h, false);

                    } else if ("POST".equals(method)) {
                        return RestClient.post(url, data, h, false);
                    }
                    return null;
                } catch (Exception e) {
                    ApiResponse apiResponse = new ApiResponse();
                    apiResponse.setStatusCode(-1);
                    apiResponse.setData(e.getLocalizedMessage().getBytes());
                    return apiResponse;
                }
            }
        };
        try {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB) {
                asyncTask.executeOnExecutor(AsyncTask.THREAD_POOL_EXECUTOR);
            } else {
                asyncTask.execute();
            }
        } catch (Exception e) {
            Log.e("callAPI", "exception", e);
        }
    }

}
