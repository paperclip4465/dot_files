GUIX_EXTRA_PROFILES=$HOME/.extra-guix-profiles

for i in $GUIX_EXTRA_PROFILES/*; do
    profile=$i/$(basename "$i")
    if [ -f "$profile"/etc/profile ]; then
	GUIX_PROFILE="$profile"
	. "$GUIX_PROFILE"/etc/profile
    fi
    unset profile
done
