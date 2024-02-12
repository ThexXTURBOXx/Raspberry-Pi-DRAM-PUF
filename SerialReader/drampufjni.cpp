#include "drampufjni.h"
#include "runnerc.h"

JNIEXPORT jstring JNICALL Java_DramPufJni_genKey
(JNIEnv* env, jobject this_obj, jstring _serial_port, jstring _gpio_chip,
 const jint _baud, const jint _rpi_power_port, const jint _sleep, jobjectArray _params,
 const jint _params_size, jstring _pos_file, const jint _key_size) {
  const auto params = new const char*[_params_size];
  for (int i = 0; i < _params_size; i++) {
    const auto str = reinterpret_cast<jstring>(env->GetObjectArrayElement(_params, i));
    params[i] = env->GetStringUTFChars(str, nullptr);
  }
  const char* serialPort = env->GetStringUTFChars(_serial_port, nullptr);
  const char* gpioChip = env->GetStringUTFChars(_gpio_chip, nullptr);
  const char* posFile = env->GetStringUTFChars(_pos_file, nullptr);

  const char* ret = gen_key(
    serialPort, gpioChip, _baud, _rpi_power_port, _sleep,
    params, _params_size, posFile, _key_size);

  env->ReleaseStringUTFChars(_serial_port, serialPort);
  env->ReleaseStringUTFChars(_gpio_chip, gpioChip);
  env->ReleaseStringUTFChars(_pos_file, posFile);

  for (int i = 0; i < _params_size; i++) {
    const auto str = reinterpret_cast<jstring>(env->GetObjectArrayElement(_params, i));
    env->ReleaseStringUTFChars(str, params[i]);
  }
  delete[] params;

  jstring jret = env->NewStringUTF(ret);
  delete[] ret;

  return jret;
}
