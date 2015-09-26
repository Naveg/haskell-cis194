runhaskell HW8Test.hs

if runhaskell Party.hs | diff correct_gl.txt -
then
    echo "Consistent Guest List"
else
    echo "Bad Guest List!!!"
fi
