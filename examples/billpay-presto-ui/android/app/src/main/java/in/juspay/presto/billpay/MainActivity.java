package in.juspay.presto.billpay;

import android.os.Build;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.webkit.WebView;
import android.widget.FrameLayout;

import in.juspay.mystique.DynamicUI;
import in.juspay.mystique.ErrorCallback;

public class MainActivity extends AppCompatActivity {

    private static final String DEV_URL = "http://192.168.39.94:8080";
    private static final String PROD_URL = "file:///android_asset/index.html";
    public static final String TAG = MainActivity.class.getName();
    private DynamicUI dynamicUI;
    private WebView mWebview ;
    private JsInterface jsInterface;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
            WebView.setWebContentsDebuggingEnabled(true);
        }

        setContentView(R.layout.activity_main);
        FrameLayout container = (FrameLayout) findViewById(R.id.dui_container);
        dynamicUI = new DynamicUI(this, container, null,new ErrorCallback() {
            @Override
            public void onError(String errorType, String errorMessage) {
            }
        });

        jsInterface = new JsInterface(this, dynamicUI);
        dynamicUI.addJavascriptInterface(jsInterface, "JBridge");

        dynamicUI.loadURL(PROD_URL);
        mWebview  = new WebView(this);

        mWebview.getSettings().setJavaScriptEnabled(true);

        mWebview.addJavascriptInterface(jsInterface,"bBridge");
        mWebview.setVisibility(View.GONE);

    }

    @Override
    protected void onPause() {
        super.onPause();
        dynamicUI.onActivityLifeCycleEvent("onPause");
    }

    @Override
    protected void onResume() {
        super.onResume();
        Log.d(TAG, "onResume() called");
        dynamicUI.onActivityLifeCycleEvent("onResume");
    }

    @Override
    public void onBackPressed() {
        dynamicUI.addJsToWebView("window.onBackPressed()");
    }
}
