clean:
	rm -fr _build *.native *.type
	find . -name "*.cmi" -type f -delete;\
	find . -name "*.cmo" -type f -delete;\
	find . -name "*.type" -type f -delete;\
	find . -name "*.cache" -type f -delete;\
	find . -name "*.log" -type f -delete

.PHONY:
	clean
