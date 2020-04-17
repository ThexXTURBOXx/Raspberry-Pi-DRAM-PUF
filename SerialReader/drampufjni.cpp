#include "drampufjni.h"
#include "runnerc.h"

JNIEXPORT jstring JNICALL Java_DramPufJni_genKey
        (JNIEnv *env, jobject this_obj, jstring _serial_port, jint _baud,
         jint _rpi_power_port, jint _sleep, jobjectArray _params,
         jint _params_size, jstring _pos_file, jint _key_size) {
    const char **params = new const char *[_params_size];
    for (int i = 0; i < _params_size; i++) {
        auto str = (jstring) env->GetObjectArrayElement(_params, i);
        params[i] = env->GetStringUTFChars(str, nullptr);
    }
    return (jstring) gen_key(
            env->GetStringUTFChars(_serial_port, nullptr),
            _baud,
            _rpi_power_port,
            _sleep,
            reinterpret_cast<const char **>(_params),
            _params_size,
            env->GetStringUTFChars(_pos_file, nullptr),
            _key_size);
}
