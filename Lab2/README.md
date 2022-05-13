# Laboratorio 2 Testsuite

En la carpeta `good` se encuentran archivos que deben ser aprovados por el type checker, mientras que en la carpeta `bad` hay programas inválidos que deben fallar.

## Pasos a seguir antes de ejecutar:
	
* [WINDOWS] cambiar línea 45 de progs-test-lab2.hs por 
>do let prog = joinPath [dir,"lab2.exe"]
* Correr 
> make
* Correr pruebas con 
> ./progs-test-lab2.exe .
* Para correr pruebas individuales 
> ./lab2.exe [path to file]
* Cuando modifiquen Typechecker o Env, correr progs-test-lab2.exe o make para compilar cambios

