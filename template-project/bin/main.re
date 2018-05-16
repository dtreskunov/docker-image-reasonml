open Core;

let main = () => {
  let message =
    List.fold([1, 2, 3, 4, 5], ~init="", ~f=(acc, x) =>
      acc ++ sprintf("%d", x)
    );
  printf("Printing a message: %s\n", message);
  printf("UTC date is: %s\n", Foo.utc_date());
};

main();
