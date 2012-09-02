* Move the content length calculated here into an option in the Cohttp
  client.  This is because Github seems to disallow chunked-posts,
  and returns a 411, so we need to force a fixed-length encoding in
  the POST.

* Detect incorrect error codes and propagate appropriately in the
  Monad. (e.g. the X-Rate-Limit header)

* Would be very useful if ATDgen could convert to/from normal string
  as well as JSON for variants.  The Github API takes strings as
  URI parameters for requests, which need to be manually converted
  at present.
