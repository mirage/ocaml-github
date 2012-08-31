Move the content length calculated here into an option in the Cohttp
client.  This is because Github seems to disallow chunked-posts,
and returns a 411, so we need to force a fixed-length encoding in
the POST.
