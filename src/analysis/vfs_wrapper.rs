use tectonic::io::IoProvider;
use vfs::Vfs;

struct VfsWrapper(Vfs);

impl IoProvider for VfsWrapper {

}