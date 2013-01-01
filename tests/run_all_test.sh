for f in tests/*.mjava
do
	./minijavac $f | grep 'Error\|File'
done
