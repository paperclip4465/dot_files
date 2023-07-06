GUIX_EXTRA_PROFILES=$HOME/.extra-guix-profiles

for i in $GUIX_EXTRA_PROFILES/*; do
	profile=$(basename "$i")
	#skip old generations
	if  [[ $profile =~ "-link" ]]; then
		continue
	fi

	if [ -f "$i"/etc/profile ]; then
		GUIX_PROFILE="$i"
		. "$GUIX_PROFILE"/etc/profile
	else
		echo "no /etc/profile found in $i"
	fi
	unset profile
done
