for f in *.mjava
do
	../minijavac $f | grep "error"
done