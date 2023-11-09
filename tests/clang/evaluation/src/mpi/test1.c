typedef struct {
  void *dummy;
} MPI_Request;

typedef struct {
  void *dummy;
} MPI_Comm;

typedef struct {
  void *dummy;
} MPI_Datatype;

typedef struct {
  void *dummy;
} MPI_Status;

extern int MPI_Isend(const void *buf, int count, MPI_Datatype datatype,
                     int dest, int tag, MPI_Comm comm, MPI_Request *request);
extern int MPI_Wait(MPI_Request *request, MPI_Status *status);
/* extern int MPI_Waitall(int count, MPI_Request array_of_requests[], */
/*                        MPI_Status *array_of_statuses); */
/* extern int MPI_Waitany(int count, MPI_Request array_of_requests[], int *index, */
/*                        MPI_Status *status); */
/* extern int MPI_Waitsome(int incount, MPI_Request array_of_requests[], */
/*                         int *outcount, int array_of_indices[], */
/*                         MPI_Status array_of_statuses[]); */


void good(const void *buf, int count, MPI_Datatype datatype,
         int dest, int tag, MPI_Comm comm) {
  MPI_Request req;
  MPI_Isend(buf, count, datatype, dest, tag, comm, &req);
  // do work
  MPI_Wait(&req, 0);
}

void bad1(const void *buf, int count, MPI_Datatype datatype,
        int dest, int tag, MPI_Comm comm, int cond) {
  MPI_Request req;

  MPI_Isend(buf, count, datatype, dest, tag, comm, &req);
  MPI_Isend(buf, count, datatype, dest, tag, comm, &req);
  MPI_Wait(&req, 0);
}

void bad2(const void *buf, int count, MPI_Datatype datatype,
        int dest, int tag, MPI_Comm comm, int cond) {
  MPI_Request req;
  MPI_Isend(buf, count, datatype, dest, tag, comm, &req);
  MPI_Wait(&req, 0);
  MPI_Isend(buf, count, datatype, dest, tag, comm, &req);
}


void bad3(const void *buf, int count, MPI_Datatype datatype,
        int dest, int tag, MPI_Comm comm, int cond) {
  MPI_Request req;
  MPI_Isend(buf, count, datatype, dest, tag, comm, &req);
  if (cond)
    MPI_Wait(&req, 0);

}
