for f in tests/*.mjava
do
	./minijavac $f | grep 'error\|File'
done
