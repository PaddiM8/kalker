#!/bin/sh

dir="./node_modules/@capacitor/android/capacitor"

# remove lines related to play service/firebase dependencies
sed -i '/playServices/d' "$dir/build.gradle"
sed -i '/firebase/d' "$dir/build.gradle"

# remove the files that rely on the proprietary services
rm -f "$dir/src/main/java/com/getcapacitor/plugin/Geolocation.java"
rm -f "$dir/src/main/java/com/getcapacitor/plugin/PushNotifications.java"
rm -f "$dir/src/main/java/com/getcapacitor/CapacitorFirebaseMessagingService.java"

# remove the references to the plugins in Bridge.java
sed -i '/Geolocation/d' "$dir/src/main/java/com/getcapacitor/Bridge.java"
sed -i '/PushNotifications/d' "$dir/src/main/java/com/getcapacitor/Bridge.java"

# remove unecessary permissions and now redundant lines from manifest
sed -i '/WAKE_LOCK/d' "$dir/src/main/AndroidManifest.xml"
sed -i '/RECEIVE_BOOT_COMPLETED/d' "$dir/src/main/AndroidManifest.xml"
i=$(grep -n "firebase_messaging_auto_init_enabled" "$dir/src/main/AndroidManifest.xml" | cut -d : -f 1)
sed -i "$i,$(($i+12))d" "$dir/src/main/AndroidManifest.xml" 2> /dev/null
