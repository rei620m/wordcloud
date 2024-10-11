from googletrans import Translator
import pandas as pd

df = pd.read_csv(my_file_path)
translator = Translator()

translation_success = 0
translation_fail = 0

def translate_text(row):
    global translation_success, translation_fail
    text_to_translate = row['Answer_OpenEnded']

    if row['PreferredLanguage'] != 'en':
        try:
            translated_text = translator.translate(text_to_translate, dest='en').text
            translation_success += 1
            return translated_text
        except Exception:
            translation_fail += 1
            return ""
    else:
        return text_to_translate

df['Answer_OpenEnded'] = df.apply(translate_text, axis=1)

print(f"translation success: {translation_success}")
print(f"translation fail: {translation_fail}")

df.to_csv(export_file_path, index=False)
